{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.Expect
import Test.Tasty.HUnit

main = defaultMainWithIngredients (expectIngredient : defaultIngredients) tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testCase "bruh" do
        pure (),
      test
        "the first test"
        [expect|first
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
g|]
            do
              pure "a\nb\nc\nd\ne\nf\ng"
        ]
    ]
