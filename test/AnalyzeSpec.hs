module AnalyzeSpec where

import qualified Data.Set as S
import Data.Time
import Database.Persist.Sqlite
import Test.Hspec

import Analyze
import Database
import EventType
import SpecCommon
import WorkDiffTime (diffWorkTime)
import qualified WorkDiffTime as WorkTime (regular)

spec :: Spec
spec = describe "analyzers" $ do
  describe "draftDuration" $ do
    context "when PR is merged" $ do
      it "returns no duration when there are no events" $ do
        draftDuration' defaultDDI `shouldBe` Nothing

      it "calculates duration from created to undraft" $ do
        let undraftTime = utcTime "2022-01-03T12:00:00Z"
            pull = defaultDDI { fEvents = [markReadyEvent undraftTime] }
        draftDuration' pull `shouldBe` Just (2.5 * nominalDay)

      it "calculates duration from draft to undraft" $ do
        let draftTime = utcTime "2022-01-02T00:00:00Z"
            undraftTime = utcTime "2022-01-04T12:00:00Z"
            pull = defaultDDI { fEvents = [markDraftEvent draftTime, markReadyEvent undraftTime] }
        draftDuration' pull `shouldBe` Just (2.5 * nominalDay)

      it "sums up multiple duration periods starting with draft event" $ do
        let times = map utcTime -- pairs of draft-undraft times
              [ "2022-01-02T00:00:00Z"
              , "2022-01-02T06:00:00Z" -- 0.25 days
              , "2022-01-02T18:00:00Z"
              , "2022-01-04T12:00:00Z" -- 1.75 days
              , "2022-01-06T00:00:00Z"
              , "2022-01-07T00:00:00Z" -- 1 day
              ]
            events = zipWith ($) (cycle [markDraftEvent, markReadyEvent]) times
            pull = defaultDDI { fEvents = events }
        draftDuration' pull `shouldBe` Just ((0.25 + 1.75 + 1) * nominalDay)

      it "sums up multiple duration periods starting with undraft event" $ do
        let times = map utcTime -- pairs of undraft-draft times
              [ "2022-01-02T00:00:00Z" -- 1 day
              , "2022-01-02T06:00:00Z"
              , "2022-01-02T18:00:00Z" -- 0.5 days
              , "2022-01-04T12:00:00Z"
              , "2022-01-06T00:00:00Z" -- 1.5 days
              ]
            events = zipWith ($) (cycle [markReadyEvent, markDraftEvent]) times
            pull = defaultDDI { fEvents = events }
        draftDuration' pull `shouldBe` Just ((1 + 0.5 + 1.5) * nominalDay)

      it "calculates duration from draft to merged" $ do
        -- even though it doesn't make much sense according to github UI
        let draftTime = utcTime "2022-01-06T00:00:00Z"
            pull = defaultDDI { fEvents = [markDraftEvent draftTime] }
        draftDuration' pull `shouldBe` Just (2 * nominalDay)

      context "when PR is in draft state" $
        it "calculates duration from created to merged" $ do
          let pull = defaultDDI { fIsDraft = True }
          draftDuration' pull `shouldBe` Just (7 * nominalDay)

    context "when PR is not merged" $ do
      let defaultUnmergedDDI = defaultDDI { fMerged = Nothing }

      it "returns no duration when there are no events" $ do
        draftDuration' defaultUnmergedDDI `shouldBe` Nothing

      it "excludes duration from draft to unmerged" $ do
        let times = map utcTime -- pairs of undraft-draft times
              [ "2022-01-02T00:00:00Z" -- 1 day
              , "2022-01-02T06:00:00Z"
              , "2022-01-02T18:00:00Z" -- 0.5 days
              , "2022-01-04T12:00:00Z"
              ]
            events = zipWith ($) (cycle [markReadyEvent, markDraftEvent]) times
            pull = defaultUnmergedDDI { fEvents = events }
        draftDuration' pull `shouldBe` Just ((1 + 0.5) * nominalDay)

  describe "ourFirstReviewLatency" $ do
    let assertTimeDiffFromCreatedToFirstReview :: EventType -> Expectation
        assertTimeDiffFromCreatedToFirstReview eventType = do
          let created = utcTime "2022-01-01T00:00:00Z"
              firstReview = utcTime "2022-01-04T12:00:00Z"
              pullId = toSqlKey 1
              events =
                [ PullEvent 01 MarkReady "" created pullId
                , PullEvent 11 eventType "" firstReview pullId
                , PullEvent 12 eventType "" (utcTime "2022-01-06T00:00:00Z") pullId
                ]
              pull = MPull { mpPull = mkPullCreated "user" created, mpEvents = events }
          ourFirstReviewLatency (S.singleton "") pull `shouldBe` Just (diffWorkTime firstReview created)

    it "returns Nothing when there are no reviews" $ do
      let pull = MPull { mpPull = mkPull "repo" 1, mpEvents = [] }
      ourFirstReviewLatency mempty pull `shouldBe` Nothing

    it "calculates time diff from PR created to first approved review" $ do
      assertTimeDiffFromCreatedToFirstReview Approved

    it "calculates time diff from PR created to first approved, yet dismissed review" $ do
      assertTimeDiffFromCreatedToFirstReview DismissedApproval

    it "calculates time diff from PR created to first commented review" $ do
      assertTimeDiffFromCreatedToFirstReview Commented

    it "calculates time diff from PR created to first requested changes review" $ do
      assertTimeDiffFromCreatedToFirstReview RequestedChanges

    it "ignores reviews from the author" $ do
      let created = utcTime "2022-01-01T00:00:00Z"
          firstReview = utcTime "2022-01-04T12:00:00Z"
          author = "user"
          pullId = toSqlKey 1
          events =
            [ PullEvent 11 Commented author (utcTime "2022-01-01T10:00:00Z") pullId
            , PullEvent 12 Approved "another" firstReview pullId
            ]
          pull = MPull { mpPull = mkPullCreated author created, mpEvents = events }
      ourFirstReviewLatency (S.singleton "another") pull `shouldBe` Just (diffWorkTime firstReview created)

    it "considers reviews from the team" $ do
      let created = utcTime "2022-01-01T00:00:00Z"
          firstReview = utcTime "2022-01-04T12:00:00Z"
          user = "user"
          pullId = toSqlKey 1
          events =
            [ PullEvent 11 Commented "anonymous" (utcTime "2022-01-01T10:00:00Z") pullId
            , PullEvent 12 Approved user firstReview pullId
            ]
          pull = MPull { mpPull = mkPullCreated "author" created, mpEvents = events }
      ourFirstReviewLatency (S.fromList [user, "author"]) pull `shouldBe` Just (diffWorkTime firstReview created)

  describe "theirFirstReviewLatency" $ do
    let assertTimeDiffFromCreatedToFirstReview :: EventType -> Expectation
        assertTimeDiffFromCreatedToFirstReview eventType = do
          let created = utcTime "2022-01-01T00:00:00Z"
              firstReview = utcTime "2022-01-04T12:00:00Z"
              pullId = toSqlKey 1
              user = "user"
              events =
                [ PullEvent 01 MarkReady "author" created pullId
                , PullEvent 11 eventType user (utcTime "2022-01-02T00:00:00Z") pullId
                , PullEvent 12 eventType "" firstReview pullId
                ]
              pull = MPull { mpPull = mkPullCreated "author" created, mpEvents = events }
          theirFirstReviewLatency (S.singleton user) pull `shouldBe` Just (diffWorkTime firstReview created)

    it "returns Nothing when there are no reviews" $ do
      let pull = MPull { mpPull = mkPull "repo" 1, mpEvents = [] }
      theirFirstReviewLatency mempty pull `shouldBe` Nothing

    it "calculates time diff from PR created to first approved review" $ do
      assertTimeDiffFromCreatedToFirstReview Approved

    it "calculates time diff from PR created to first approved, yet dismissed review" $ do
      assertTimeDiffFromCreatedToFirstReview DismissedApproval

    it "calculates time diff from PR created to first commented review" $ do
      assertTimeDiffFromCreatedToFirstReview Commented

    it "calculates time diff from PR created to first requested changes review" $ do
      assertTimeDiffFromCreatedToFirstReview RequestedChanges

    it "ignores reviews from the author" $ do
      let created = utcTime "2022-01-01T00:00:00Z"
          firstReview = utcTime "2022-01-04T12:00:00Z"
          author = "user"
          pullId = toSqlKey 1
          events =
            [ PullEvent 11 Commented author (utcTime "2022-01-01T10:00:00Z") pullId
            , PullEvent 12 Approved "another" firstReview pullId
            ]
          pull = MPull { mpPull = mkPullCreated author created, mpEvents = events }
      theirFirstReviewLatency (S.singleton "author") pull `shouldBe` Just (diffWorkTime firstReview created)

    it "ignores reviews from the team" $ do
      let created = utcTime "2022-01-01T00:00:00Z"
          firstReview = utcTime "2022-01-04T12:00:00Z"
          user = "user"
          pullId = toSqlKey 1
          events =
            [ PullEvent 11 Commented user (utcTime "2022-01-01T10:00:00Z") pullId
            , PullEvent 12 Approved "anonymous" firstReview pullId
            ]
          pull = MPull { mpPull = mkPullCreated "author" created, mpEvents = events }
      theirFirstReviewLatency (S.fromList [user, "author"]) pull `shouldBe` Just (diffWorkTime firstReview created)

draftDuration' :: (HasCallStack, DraftDurationInput a) => a -> Maybe NominalDiffTime
draftDuration' = fmap WorkTime.regular . draftDuration

defaultDDI :: HasCallStack => FreeDDI
defaultDDI = FreeDDI
  { fCreated = utcTime "2022-01-01T00:00:00Z"
  , fMerged = Just $ utcTime "2022-01-08T00:00:00Z"
  , fEvents = []
  , fIsDraft = False
  }

markDraftEvent :: UTCTime -> PullEvent
markDraftEvent time = PullEvent
  { pullEventGhId = 0
  , pullEventType = MarkDraft
  , pullEventActor = "user"
  , pullEventCreated = time
  , pullEventPull = toSqlKey 0
  }

markReadyEvent :: UTCTime -> PullEvent
markReadyEvent time = PullEvent
  { pullEventGhId = 0
  , pullEventType = MarkReady
  , pullEventActor = "user"
  , pullEventCreated = time
  , pullEventPull = toSqlKey 0
  }

data FreeDDI = FreeDDI
  { fCreated :: UTCTime
  , fMerged :: Maybe UTCTime
  , fEvents :: [PullEvent]
  , fIsDraft :: Bool
  }

instance DraftDurationInput FreeDDI where
  ddiCreated = fCreated
  ddiMerged = fMerged
  ddiEvents = fEvents
  ddiIsDraft = fIsDraft
