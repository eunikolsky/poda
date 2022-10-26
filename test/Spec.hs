import Data.Functor.Identity
import Data.Time.Clock (NominalDiffTime)
import Database.Persist.Sql (toSqlKey)
import Test.Hspec
import qualified Data.HashMap.Strict as HM

import Analyze (adjacentPairs)
import Database
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
            [ PullEvent 42 DismissedApproval "user" (utcTime "2022-09-06T16:57:34Z") pullId
            , PullEvent 100 RequestedChanges "user1" (utcTime "2022-09-07T06:32:17Z") pullId
            , PullEvent 200 Approved "user2" (utcTime "2022-09-09T07:37:58Z") pullId
            , PullEvent 333 Commented "user1" (utcTime "2022-09-09T23:06:31Z") pullId
            ]
      parsePullTimelineEvents pullId timelineEventsString `shouldBe` Right timelineEvents

  describe "mergePREvents" $ do
    it "orders merged events by increasing creation time" $ do
      let pullId = toSqlKey 1
          events0 =
            [ PullEvent 0 MarkDraft "" (utcTime "2022-01-01T02:00:00Z") pullId
            , PullEvent 2 DismissedApproval "" (utcTime "2022-01-02T10:00:00Z") pullId
            , PullEvent 4 Approved "" (utcTime "2022-01-12T19:30:00Z") pullId
            ]
          events1 =
            [ PullEvent 1 Commented "" (utcTime "2022-01-01T12:00:00Z") pullId
            , PullEvent 3 MarkReady "" (utcTime "2022-01-12T00:00:00Z") pullId
            ]
          isOrderedByCreationTime = all (\(e0, e1) -> pullEventCreated e0 <= pullEventCreated e1)
            . adjacentPairs

      mergePREvents events0 events1 `shouldSatisfy` isOrderedByCreationTime

  describe "describeReviewActors" $ do
    it "shows names alphabetically ordered" $ do
      let actors = HM.fromList [("user", 1), ("author", 1), ("reviewer", 1)]
      describeReviewActors actors `shouldBe` "`author`, `reviewer`, `user`"

    it "shows names ordered by number of reviews" $ do
      let actors = HM.fromList [("user", 4), ("author", 4), ("reviewer", 1), ("bob", 2)]
      describeReviewActors actors `shouldBe` "`author`×4, `user`×4, `bob`×2, `reviewer`"

  describe "formatDiffTime" $ do
    it "shows weeks to seconds" $ do
      let duration = mkDuration 10 6 10 10
      formatDiffTime duration `shouldBe` "10w 6d 10:10"

    it "doesn't show weeks when 0 weeks" $ do
      let duration = mkDuration 0 6 10 10
      formatDiffTime duration `shouldBe` "6d 10:10"

    it "doesn't show days when 0 days" $ do
      let duration = mkDuration 8 0 10 10
      formatDiffTime duration `shouldBe` "8w 10:10"

    it "prepends zero to one-digit hour" $ do
      let duration = mkDuration 0 0 1 10
      formatDiffTime duration `shouldBe` "01:10"

    it "prepends zero to one-digit minute" $ do
      let duration = mkDuration 0 0 10 1
      formatDiffTime duration `shouldBe` "10:01"

  WorkDiffTimeSpec.spec
  AnalyzeSpec.spec

mkDuration :: Int -> Int -> Int -> Int -> NominalDiffTime
mkDuration weeks days hours minutes =
  fromIntegral $ ((((((weeks * 7) + days) * 24) + hours) * 60) + minutes) * 60
