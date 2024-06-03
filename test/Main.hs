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
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = do
  _ <- Exception.try @Exception.SomeException $ defaultMainWithIngredients (expectIngredient : defaultIngredients) tests
  pure ()

tests =
  testGroup
    "Tests"
    [ escapingTests,
      expectTests
    ]

expectTests =
  testGroup
    "Expect"
    [ test
        "the first test"
        [expect|first
second
third
fourth|]
        do
          pure "first\nsecond\nthird\nfourth",
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
    [ test "Simple" [expect|[Text "aasdf ",QuoteEnd 0,Text "|",Text "[|",Text "[",QuoteEnd 1,Text " asas~~~]",QuoteEnd 3,Text ""]|] do
        pure $ T.pack $ show $ lexTokens "aasdf |]|[|[|~] asas~~~]|~~~]",
      test "Single" [expect|[Text "",QuoteEnd 0,Text ""]|] do
        pure $ T.pack $ show $ lexTokens "|]",
      test "Escape Expect" [expect|"|~~] |~~~] |~~] [| [[[]]]] [|~~~~~] ~~~]["|] do
        pure $ T.pack $ show $ escape "|] |~] |] [| [[[]]]] [|~~~] ~~~][",
      test "Works with |]" [expect||~~] |~]|] do
        pure "|~] |]",
      QC.testProperty "tokensToText . lexTokens = id" \s ->
        tokensToText (lexTokens (T.pack s)) == (T.pack s),
      QC.testProperty "unescape . escape = id" \s ->
        unescape (escape (T.pack s)) == (T.pack s)
    ]
    
