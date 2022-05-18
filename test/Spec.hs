{-# LANGUAGE OverloadedStrings #-}

import Data.Functor.Identity
import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
  describe "parseRelLink" $ do
    it "returns the found link" $ do
      let input = "<anonymous_url>; rel=\"next\""
      parseRelLink "next" input `shouldBe` Just "anonymous_url"

    it "finds the link in a list" $ do
      let input = "<anonymous_url>; rel=\"prev\", <next_url>; rel=\"next\""
      parseRelLink "next" input `shouldBe` Just "next_url"

    it "returns Nothing when the link is not found" $
      parseRelLink "next" "<anonymous_url>; rel=\"prev\"" `shouldBe` Nothing

    it "returns Nothing when source string is empty" $
      parseRelLink "next" "" `shouldBe` Nothing

  describe "unfoldrM" $ do
    it "returns the first value" $ do
      let f = Identity . const ([0, 1], Nothing)
      runIdentity (unfoldrM f 0) `shouldBe` [0, 1]

    it "returns the combined value" $ do
      let f x = Identity ("ab", if x <= 1 then Just (x + 1) else Nothing)
      runIdentity (unfoldrM f 0) `shouldBe` "ababab"

    it "combines values to the right" $ do
      let f x = Identity ([x, x + 1], if x <= 3 then Just (x + 2) else Nothing)
      runIdentity (unfoldrM f 0) `shouldBe` [0, 1, 2, 3, 4, 5]
