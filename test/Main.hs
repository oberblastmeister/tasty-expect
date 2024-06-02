{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import Control.Exception qualified as Exception
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.Expect
import Test.Tasty.Expect.Internal
import Test.Tasty.HUnit

main :: IO ()
main = do
  _ <- Exception.try @Exception.SomeException $ defaultMainWithIngredients (expectIngredient : defaultIngredients) tests
  pure ()

tests =
  testGroup
    "Tests"
    [ escapingTests,
      testCase "bruh" do
        pure (),
      test
        "the first test"
        [expect|first
second
third
fourth|]
        do
          pure "first\nsecond\nthird\nfourth",
      testCase "another" do
        pure (),
      test
        "the second test"
        [expect|bruh|]
        do
          pure "bruh",
      testGroup
        "nested"
        [ testCase "nested 1" do
            pure (),
          test
            "nested 2"
            [expect|a
b
c
d
e
f
g|]
            do
              pure "a\nb\nc\nd\ne\nf\ng"
        ]
    ]

escapingTests =
  testGroup
    "Escaping"
    [ test "Simple" [expect||] do
        pure $ T.pack $ show $ lexTokens "aasdf |]|[|[|~] asas~~~]|~~~]"
    ]
