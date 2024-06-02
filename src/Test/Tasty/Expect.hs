{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Tasty.Expect
  ( expectIngredient,
    expect,
    test,
    updateExpects,
  )
where

import Control.Exception qualified as Exception
import Control.Monad (unless)
import Control.Monad qualified as Monad
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B.Char8
import Data.ByteString.Lazy qualified as BL
import Data.Char qualified as Char
import Data.Foldable (for_)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import Data.Traversable (for)
import Data.Typeable (Typeable)
import Data.Typeable qualified as Typeable
import GHC.Stack (HasCallStack)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Options.Applicative qualified as O
import System.IO qualified as IO
import System.IO.Temp (withSystemTempFile)
import System.Process qualified as P
import System.Process.Typed
import Test.Tasty.Expect.Internal
import Test.Tasty.Ingredients qualified as Tasty
import Test.Tasty.Options qualified as Tasty
import Test.Tasty.Providers qualified as Tasty
import Test.Tasty.Providers.ConsoleFormat qualified as Tasty
import Test.Tasty.Runners qualified as Tasty

data Expect = Expect
  { expectContents :: String,
    expectFile :: String,
    -- (line, char)
    expectStart :: (Int, Int),
    expectEnd :: (Int, Int)
  }
  deriving (Show, Eq, Ord)

expect :: TH.QuasiQuoter
expect =
  TH.QuasiQuoter
    { TH.quoteExp =
        \contents -> do
          loc <- TH.location
          let contentsExp = TH.liftString contents
          let file = TH.lift $ TH.loc_filename loc
          let start = TH.lift $ TH.loc_start loc
          let end = TH.lift $ TH.loc_end loc
          [|
            let ex =
                  Expect
                    { expectContents = $contentsExp,
                      expectFile = $file,
                      expectStart = $start,
                      expectEnd = $end
                    }
             in ex
            |],
      TH.quotePat = \_ -> error "expect: quotePat not implemented",
      TH.quoteType = \_ -> error "expect: quoteType not implemented",
      TH.quoteDec = \_ -> error "expect: quoteDec not implemented"
    }

test :: String -> Expect -> IO Text -> Tasty.TestTree
test name ex actual = Tasty.singleTest name $ ExpectTest ex actual

data ExpectTest = ExpectTest !Expect !(IO Text)
  deriving (Typeable)

instance Tasty.IsTest ExpectTest where
  testOptions = pure []

  run options (ExpectTest expect action) _progress = do
    actual <- action
    let exContents = unescape (T.pack (expectContents expect))
    if exContents == (actual)
      then pure $ Tasty.testPassed ""
      else
        pure $
          Tasty.testFailedDetails
            "Expected did not match actual"
            ( Tasty.ResultDetailsPrinter $
                \indent consoleFormat -> do
                  output <- diffString (T.unpack exContents) (T.unpack actual) runDelta
                  output <- pure $ BL.toStrict output
                  let lines = B.Char8.lines output
                  let indentation = B.Char8.replicate (2 + 2 * indent) ' '
                  let lines' = fmap (indentation <>) lines
                  let output' = B.Char8.unlines lines' <> "\n"
                  B.putStr output'
            )

diffString :: String -> String -> (FilePath -> FilePath -> IO a) -> IO a
diffString s s' f = do
  withSystemTempFile "expect" $ \fp h -> do
    IO.hPutStr h (s ++ "\n")
    IO.hClose h
    withSystemTempFile "expect" $ \fp' h' -> do
      IO.hPutStr h' (s' ++ "\n")
      IO.hClose h'
      f fp fp'

runGitDiff :: FilePath -> FilePath -> IO ()
runGitDiff p p' = do
  _ <-
    Exception.try @Exception.SomeException $
      P.callProcess
        "git"
        [ "-c",
          "color.diff=always",
          "--no-pager",
          "diff",
          "--no-index",
          p,
          p'
        ]
  _ <-
    Exception.try @Exception.SomeException $
      P.callProcess
        "delta"
        [p, p']
  pure ()

runDelta :: FilePath -> FilePath -> IO BL.ByteString
runDelta p p' = do
  (exit, stdout, _) <- readProcess $ deltaProc p p'
  pure stdout

deltaProc p p' = proc "delta" [p, p']

runGitDiff' :: FilePath -> FilePath -> IO String
runGitDiff' p p' = do
  -- P.readProcess "git" ["-c", "color.diff=always", "--no-pager", "diff", "--no-index", p, p'] ""
  -- P.readProcess "eza" ["--color=always", "src"] ""
  (_, stdout, _) <- P.readProcessWithExitCode "delta" [p, p'] ""
  -- (_, stdout, _) <- P.readProcessWithExitCode "git" ["-c", "color.diff=always", "--no-pager", "diff", "--no-index", p, p'] ""
  pure stdout

newtype ExpectOption = ExpectOption Bool

instance Tasty.IsOption ExpectOption where
  defaultValue = ExpectOption False
  parseValue = fmap ExpectOption . safeReadBool
  optionName = return "update-expect"
  optionHelp = return "Update expect tests"
  optionCLParser = Tasty.mkFlagCLParser (O.short 'u') (ExpectOption True)

assertIO :: (HasCallStack) => Bool -> IO ()
assertIO x =
  unless x $ error "assertion failed"

applyPatch :: Expect -> Text -> Text -> IO Text
applyPatch ex replace fileContents = do
  let (subtract 1 -> startLine, subtract 1 -> startChar) = expectStart ex
      (subtract 1 -> endLine, subtract 1 -> endChar) = expectEnd ex
  let fileLines = fmap (<> "\n") (T.lines fileContents)
  let fileLinesLen = length fileLines
  assertIO $ startLine <= endLine
  assertIO $ startLine < fileLinesLen
  let (before, after) = splitAt startLine fileLines
  res <- case after of
    (lineStart : linesAfterStart) -> do
      assertIO (startChar < T.length lineStart)
      let (prev, rest) = T.splitAt startChar lineStart
      -- TODO: do some extra checks here
      pure $
        T.concat before
          <> prev
          <> replace
          <> fromJust
            ( T.stripPrefix
                (T.pack (expectContents ex))
                (rest <> T.concat linesAfterStart)
            )
    _ -> error "wrong"
  pure res

expectIngredient :: Tasty.Ingredient
expectIngredient = Tasty.TestManager [Tasty.Option (Proxy @ExpectOption)] $ \options testTree ->
  do
    let (ExpectOption b) = Tasty.lookupOption options
    if b
      then
        Just
          ( do
              updateExpects options testTree
              pure True
          )
      else Nothing

updateExpects :: Tasty.OptionSet -> Tasty.TestTree -> IO ()
updateExpects options testTree = do
  let expects = collectExpectTests options testTree
  patches <- for expects $ \(ExpectTest ex act) -> do
    patch <- act
    pure (ex, escape patch)
  let patchesByFile = Map.fromListWith (<>) $ fmap (\(ex, contents) -> (expectFile ex, [(ex, contents)])) patches
  for_ (Map.toList patchesByFile) $ \(filePath, patches) -> do
    fileContents <- T.IO.readFile filePath
    let patchesSorted =
          List.sortBy
            ( \(ex1, _) (ex2, _) ->
                let s1 = expectStart ex1
                    s2 = expectStart ex2
                 in compare (fst s1) (fst s1) <> compare (snd s2) (snd s2)
            )
            patches
    newFileContents <-
      Monad.foldM
        (\acc (ex, patch) -> applyPatch ex patch acc)
        fileContents
        patchesSorted
    Monad.when (fileContents /= newFileContents) $ do
      T.IO.writeFile filePath newFileContents

collectExpectTests :: Tasty.OptionSet -> Tasty.TestTree -> [ExpectTest]
collectExpectTests options testTree =
  Tasty.foldTestTree
    ( Tasty.trivialFold
        { Tasty.foldSingle = \_options _name test ->
            case Typeable.cast @_ @ExpectTest test of
              Nothing -> []
              Just ex -> [ex]
        }
    )
    options
    testTree

safeReadBool :: String -> Maybe Bool
safeReadBool s =
  case map Char.toLower s of
    "true" -> Just True
    "false" -> Just False
    _ -> Nothing
