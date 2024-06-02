{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module Test.Hspec.Expect
  ( expect,
    assertEq,
    Expect (..),
    ExpectExample (..),
    test,
  )
where

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar qualified as MVar
import Control.Monad (unless)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import System.Environment (lookupEnv)
import System.IO qualified as IO
import System.IO.Temp (withSystemTempFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Process qualified as P
import Test.Hspec.Core.Spec

-- keep track of a map from files to contents and patches that were applied
-- when applying a new patch, make sure to map from the previous patches
data Runtime = Runtime
  { hit :: !Int
  }

rt :: MVar Runtime
rt = unsafePerformIO $ MVar.newMVar (Runtime {hit = 0})
{-# NOINLINE rt #-}

assertIO :: (HasCallStack) => Bool -> IO ()
assertIO x =
  unless x $ error "assertion failed"

-- if the expect is [expect|asdpfoiuasdfpoiausdfpaoiu|]
-- the start position is pointing at the character right after the bar '|'
-- also, the positions are one based and inclusive
update :: Expect -> Text -> Runtime -> IO ()
update expect new rt = do
  let (subtract 1 -> startLine, subtract 1 -> startChar) = expectStart expect
      (subtract 1 -> endLine, subtract 1 -> endChar) = expectEnd expect
  fileContents <- T.IO.readFile (expectFile expect)
  let fileLines = fmap (<> "\n") (T.lines fileContents)
  let fileLinesLen = length fileLines
  assertIO $ startLine <= endLine
  assertIO $ startLine < fileLinesLen
  let (before, after) = splitAt startLine fileLines
  res <- case after of
    (lineStart : linesAfterStart) -> do
      assertIO (startChar < T.length lineStart)
      let (prev, rest) = T.splitAt startChar lineStart
      -- putStrLn $ "lineStart: " <> show lineStart
      -- putStrLn $ "rest: " <> show rest
      pure $
        T.concat before
          <> prev
          <> new
          <> fromJust
            ( T.stripPrefix
                (T.pack (expectContents expect))
                (rest <> T.concat linesAfterStart)
            )
    _ -> error "wrong"
  T.IO.writeFile (expectFile expect) res

diffString :: String -> String -> IO String
diffString s s' = do
  withSystemTempFile "expect" $ \fp h -> do
    IO.hPutStr h (s ++ "\n")
    IO.hClose h
    withSystemTempFile "expect" $ \fp' h' -> do
      IO.hPutStr h' (s' ++ "\n")
      IO.hClose h'
      runGitDiff' fp fp'

runGitDiff' :: FilePath -> FilePath -> IO String
runGitDiff' p p' = do
  -- P.readProcess "git" ["-c", "color.diff=always", "--no-pager", "diff", "--no-index", p, p'] ""
  -- P.readProcess "eza" ["--color=always", "src"] ""
  (_, stdout, _) <- P.readProcessWithExitCode "delta" [p, p'] ""
  -- (_, stdout, _) <- P.readProcessWithExitCode "git" ["-c", "color.diff=always", "--no-pager", "diff", "--no-index", p, p'] ""
  pure stdout

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

assertEq :: (HasCallStack) => Expect -> String -> IO Result
assertEq ex@Expect {..} actual = do
  putStrLn "hello world"
  if expectContents == actual
    then do 
      MVar.modifyMVar_ rt $ \rt -> do
        putStrLn $ "hit: " ++ show (hit rt)
        pure (rt {hit = hit rt + 1})
      return (Result "" Success)
    else do
      MVar.modifyMVar rt $ \rt -> do
        putStrLn $ "hit: " ++ show (hit rt)
        shouldUpdate <- lookupEnv "UPDATE_EXPECT"
        case shouldUpdate of
          Just _ -> do
            update ex (T.pack actual) rt
            pure (rt {hit = hit rt + 1}, Result "" Success)
          Nothing -> do
            res <- diffString expectContents actual
            pure
              ( rt {hit = hit rt + 1},
                Result
                  { resultInfo = "Expected did not match actual",
                    resultStatus =
                      Failure
                        Nothing
                        (ColorizedReason res)
                  }
              )

data ExpectExample = ExpectExample !Expect (IO Text)

instance Example ExpectExample where
  type Arg ExpectExample = ()
  evaluateExample e = evaluateExample (\() -> e)

instance Example (IO ExpectExample) where
  type Arg (IO ExpectExample) = ()
  evaluateExample e = evaluateExample (\() -> e)

instance Example (arg -> IO ExpectExample) where
  type Arg (arg -> IO ExpectExample) = arg
  evaluateExample expectExampleWithArg _ action _ = do
    ref <- newIORef (Result "" Success)
    action $ \arg -> do
      r <- runExpectExample =<< expectExampleWithArg arg
      writeIORef ref r
    readIORef ref

instance Example (arg -> ExpectExample) where
  type Arg (arg -> ExpectExample) = arg
  evaluateExample expectExampleWithArg _ action _ = do
    ref <- newIORef (Result "" Success)
    action $ \arg -> do
      r <- runExpectExample $ expectExampleWithArg arg
      writeIORef ref r
    readIORef ref

runExpectExample :: ExpectExample -> IO Result
runExpectExample (ExpectExample ex actual) = do
  actual' <- actual
  assertEq ex (T.unpack actual')

test :: Expect -> IO Text -> SpecWith (Arg ExpectExample)
test ex actual = it "expect" (ExpectExample ex actual)
