{-# LANGUAGE OverloadedStrings #-}

import Data.Functor.Identity
import Data.Text (Text)
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Test.Hspec

import Database
import Database.Persist.Sql (toSqlKey)
import EventType
import Lib
import AnalyzeSpec
import TestData
import WorkDiffTimeSpec
import SpecCommon

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
      runIdentity (unfoldrM f (0 :: Int)) `shouldBe` [0, 1 :: Int]

    it "returns the combined value" $ do
      let f x = Identity ("ab", if x <= 1 then Just (x + 1) else Nothing)
      runIdentity (unfoldrM f (0 :: Int)) `shouldBe` ("ababab" :: String)

    it "combines values to the right" $ do
      let f x = Identity ([x, x + 1], if x <= 3 then Just (x + 2) else Nothing)
      runIdentity (unfoldrM f 0) `shouldBe` [0, 1, 2, 3, 4, 5 :: Int]

  describe "sortByRepoAndNumberDesc" $ do
    it "orders by repo first" $ do
      let repos = [mkPull "c" 0, mkPull "a" 0, mkPull "b" 0, mkPull "a" 0]
      pullRepo <$> sortByRepoAndNumberDesc repos `shouldBe` ["a", "a", "b", "c"]

    it "orders by decreasing number within the same repo" $ do
      let repos = [mkPull "c" 0, mkPull "a" 0, mkPull "b" 0, mkPull "a" 8, mkPull "c" 42]
      let expectedRepos = [mkPull "a" 8, mkPull "a" 0, mkPull "b" 0, mkPull "c" 42, mkPull "c" 0]
      sortByRepoAndNumberDesc repos `shouldBe` expectedRepos

  describe "timeline events response" $ do
    it "is parsed from JSON" $ do
      let pullId = toSqlKey 1
          timelineEvents =
            [ PullEvent 42 DismissedApproval (utcTime "2022-09-06T16:57:34Z") pullId
            , PullEvent 100 RequestedChanges (utcTime "2022-09-07T06:32:17Z") pullId
            , PullEvent 200 Approved (utcTime "2022-09-09T07:37:58Z") pullId
            , PullEvent 333 Commented (utcTime "2022-09-09T23:06:31Z") pullId
            ]
      parsePullTimelineEvents pullId timelineEventsString `shouldBe` Right timelineEvents

  WorkDiffTimeSpec.spec
  AnalyzeSpec.spec

mkPull :: Text -> Int -> Pull
mkPull repo number = Pull
    repo
    number
    ""
    ""
    ""
    False
    (UTCTime (fromOrdinalDate 2000 1) (secondsToDiffTime 0))
    Nothing
    ""
