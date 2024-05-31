{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

-- |
-- Module      : Test.Hspec.Golden
-- Description : Golden tests for Hspec
-- Copyright   : Stack Builders (c), 2019-2020
-- License     : MIT
-- Maintainer  : cmotoche@stackbuilders.com
-- Stability   : experimental
-- Portability : portable
--
-- Golden tests store the expected output in a separated file. Each time a golden test
-- is executed the output of the subject under test (SUT) is compared with the
-- expected output. If the output of the SUT changes then the test will fail until
-- the expected output is updated. We expose 'defaultGolden' for output of
-- type @String@. If your SUT has a different output, you can use 'Golden'.
module Test.Hspec.Expect
  ( expect,
    assertEq,
    Expect (..),
  )
where

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar qualified as MVar
import Control.Exception
import Control.Monad (unless)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import GHC.Stack (HasCallStack)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import System.Environment (lookupEnv)
import System.IO qualified as IO
import System.IO.Temp (withSystemTempFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Process.Typed

data Runtime = Runtime

rt :: MVar Runtime
rt = unsafePerformIO $ MVar.newMVar Runtime
{-# NOINLINE rt #-}

changeList :: (Monad m) => (Int, Int) -> ([a] -> m [a]) -> [a] -> m [a]
changeList pos swap list = do
  let (before, stuff) = splitAt (fst pos) list
  let (stuff', after) = splitAt (snd pos - fst pos + 1) stuff
  x <- swap stuff'
  pure $ before <> x <> after

changeText :: (Monad m) => (Int, Int) -> (Text -> m Text) -> Text -> m Text
changeText pos swap text = do
  let (x, rest) = T.splitAt (fst pos) text
  let (rest', y) = T.splitAt (snd pos - fst pos + 1) rest
  res <- swap rest'
  pure $ x <> res <> y

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
      putStrLn $ "lineStart: " <> show lineStart
      putStrLn $ "rest: " <> show rest
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

-- res <- do
--   assertIO $ startLine <= endLine
--   assertIO $ (startLine < fileLinesLen)
--   assertIO (endLine < fileLinesLen)
-- changeList
--   (startLine, endLine)
--   ( \lines -> do
--       if length lines == 1
--         then do
--           res <- changeText (startChar, endChar) (\_ -> pure new) (head lines)
--           pure [res]
--         else do
--           let ([firstLine], lines') = splitAt 1 lines
--           let (_middleLines, [lastLine]) = splitAt (length lines' - 1) lines'
--           putStrLn $ "startChar: " ++ show startChar
--           putStrLn $ "len: " ++ show (T.length firstLine)
--           assertIO (startChar < T.length firstLine)
--           assertIO (endChar < T.length lastLine)
--           let firstLine' = T.take startChar firstLine
--           let lastLine' = T.drop (endChar + 1) lastLine
--           pure [firstLine', new <> T.pack "|]", lastLine']
--   )
--   fileLines

diffString :: String -> String -> IO ()
diffString s s' = do
  withSystemTempFile "expect" $ \fp h -> do
    IO.hPutStr h (s ++ "\n")
    IO.hClose h
    withSystemTempFile "expect" $ \fp' h' -> do
      IO.hPutStr h' (s' ++ "\n")
      IO.hClose h'
      runGitDiff fp fp'

runGitDiff :: FilePath -> FilePath -> IO ()
runGitDiff p p' = do
  let gitConfig = setStderr inherit $ setStdout inherit $ gitDiffProc p p'
  code <- runProcess gitConfig
  case code of
    ExitSuccess -> return ()
    (ExitFailure 1) -> return ()
    _ -> throwIO $ userError "git diff did not return 0 or 1"

gitDiffProc :: FilePath -> FilePath -> ProcessConfig () () ()
gitDiffProc p p' =
  -- proc "git" ["-c", "color.diff=always", "--no-pager", "diff", "--no-index", p, p']
  proc "delta" [p, p']

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

assertEq :: (HasCallStack) => Expect -> String -> IO ()
assertEq ex@Expect {..} actual = do
  if expectContents == actual
    then return ()
    else do
      MVar.withMVar rt $ \rt -> do
        shouldUpdate <- lookupEnv "UPDATE_EXPECT"
        case shouldUpdate of
          Just _ -> do
            update ex (T.pack actual) rt
          Nothing -> do
            diffString expectContents actual
            evaluate $ error "expect differed with actual"
