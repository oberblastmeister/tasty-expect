{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ExpectSpec (spec) where

import Test.Hspec
import Test.Hspec.Expect

spec = parallel do
  describe "first" do
    test
      [expect|bruh|]
      do
        pure "bruh"

    test
      [expect|bruh|]
      do
        1 `shouldBe`2
        pure "bruh"