module Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Lambda

main :: IO ()
main = hspec $ do
  describe "Lambda" $ do
    it "should" $ do
      head [23 ..] `shouldBe` (23 :: Int)
