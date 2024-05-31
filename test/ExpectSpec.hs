{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ExpectSpec (spec) where

import Test.Hspec
import Test.Hspec.Expect

-- new interface
-- test [expect||] do
--   pure "hello world"

spec = do
  describe "first" do
    it "testing" do
      let ex =
            [expect|
hello world bruh what is this|]
      print "testing"
      assertEq ex "bruh"
    it "another" do
      print "another"
      let ex = [expect||]
      assertEq ex "asdfasdfasdfadsf"
