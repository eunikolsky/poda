{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Analyze
  ( DraftDurationInput(..)
  , MPull(..)
  , ReviewAnalysis(..)
  , adjacentPairs
  , draftDuration
  , ourFirstReview
  , theirFirstReview
  ) where

import Control.Applicative ((<|>))
import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import GHC.Stack (HasCallStack)
import qualified Data.Set as S

import Database
import EventType
import WorkDiffTime hiding (regular, work)

-- | Defines the necessary data to calculate the draft duration.
class DraftDurationInput a where
  ddiCreated :: a -> UTCTime
  ddiMerged :: a -> Maybe UTCTime
  ddiEvents :: a -> [PullEvent]
  -- | Describes the /current/ draft state of the PR, after all events.
  ddiIsDraft :: a -> Bool

-- | Calculates the draft duration of a PR.
-- When a PR is marked as draft or ready for review, an event is created;
-- we don't know the state of the PR at a point in time until we get an
-- event after that time.
--
-- Warnings:
-- * according to github's UI, it's not possible to merge a draft PR; however,
-- @git@ doesn't care about PRs and so can merge the branch and close the PR,
-- and /maybe/ the github API can do the same.
--
-- Assumptions:
-- * there can't be 2+ events of the same type in a row.
draftDuration :: (HasCallStack, DraftDurationInput a) => a -> Maybe WorkDiffTime
draftDuration ddi = foldMap pairDuration $ adjacentPairs stateTransitions
  where
    stateTransitions = catMaybes
      $ Just (StateTransition Created (ddiCreated ddi))
      : map parseStateEvent (ddiEvents ddi)
      ++ [StateTransition Merged <$> ddiMerged ddi]

    parseStateEvent :: PullEvent -> Maybe StateTransition
    parseStateEvent e
      =   StateTransition MarkedDraft <$> markDraftTime e
      <|> StateTransition MarkedReady <$> markReadyTime e

    pairDuration :: HasCallStack => (StateTransition, StateTransition) -> Maybe WorkDiffTime
    pairDuration (StateTransition MarkedDraft draft, StateTransition MarkedReady ready) = Just $ diffWorkTime ready draft
    pairDuration (StateTransition MarkedReady _, StateTransition MarkedDraft _) = Nothing
    pairDuration (StateTransition MarkedDraft draft, StateTransition Merged merged) = Just $ diffWorkTime merged draft
    pairDuration (StateTransition MarkedReady _, StateTransition Merged _) = Nothing
    pairDuration (StateTransition Created created, StateTransition MarkedReady ready) = Just $ diffWorkTime ready created
    pairDuration (StateTransition Created created, StateTransition Merged merged) =
      if ddiIsDraft ddi then Just (diffWorkTime merged created) else Nothing
    pairDuration (StateTransition Created _, StateTransition MarkedDraft _) = Nothing
    pairDuration (x, c@(StateTransition Created _)) = error . mconcat $ ["pairDuration: impossible ", show c, " after ", show x]
    pairDuration (m@(StateTransition Merged _), x) = error . mconcat $ ["pairDuration: impossible ", show x, " after ", show m]
    pairDuration (x@(StateTransition MarkedDraft _), y@(StateTransition MarkedDraft _)) = error . mconcat $ ["pairDuration: unexpected pair of MarkedDraft in a row: ", show (x, y)]
    pairDuration (x@(StateTransition MarkedReady _), y@(StateTransition MarkedReady _)) = error . mconcat $ ["pairDuration: unexpected pair of MarkedReady in a row: ", show (x, y)]

-- | Describes state transition events that are interesting for @draftDuration@.
data StateTransitionEvent = Created | MarkedDraft | MarkedReady | Merged
  deriving Show

data StateTransition = StateTransition
  { stEvent :: StateTransitionEvent
  , stTime :: UTCTime
  }
  deriving Show

adjacentPairs :: [a] -> [(a, a)]
adjacentPairs [] = []
adjacentPairs xs = zip xs (tail xs)

-- | A model @Pull@ with its events.
-- (It doesn't seem possible to get a @Pull@ with all its events from @persist@)
data MPull = MPull
  { mpPull :: Pull
  , mpEvents :: [PullEvent]
  -- ^ Events associated with the `mpPull`, _must be in the older to newer order_.
  }
  deriving Show

instance DraftDurationInput MPull where
  ddiCreated = pullCreated . mpPull
  ddiMerged = pullMerged . mpPull
  ddiEvents = mpEvents
  ddiIsDraft = pullIsDraft . mpPull

-- | Analysis of a review: how long it took since its PR was open and who did
-- the review.
data ReviewAnalysis = ReviewAnalysis
  { raLatency :: !WorkDiffTime
  -- ^ Length of time between a PR is created and the review.
  , raActor :: !Text
  }
  deriving (Eq, Show)

-- | Finds the first review by someone on the `team` (excluding the PR author).
ourFirstReview :: Set Text -> MPull -> Maybe ReviewAnalysis
ourFirstReview team = firstReviewLatency (`S.member` team)

-- | Finds the first review by someone _not_ on the `team` (excluding the PR author).
theirFirstReview :: Set Text -> MPull -> Maybe ReviewAnalysis
theirFirstReview team = firstReviewLatency (`S.notMember` team)

type Predicate a = a -> Bool

firstReviewLatency :: Predicate Text -> MPull -> Maybe ReviewAnalysis
firstReviewLatency shouldUseEventByActor MPull{mpPull=Pull{..},mpEvents} = ReviewAnalysis
  <$> (diffWorkTime <$> fmap pullEventCreated firstReview <*> pure pullCreated)
  <*> (pullEventActor <$> firstReview)

  where
    firstReview = find (\PullEvent{..} ->
        isReviewEventType pullEventType
        && shouldUseEventByActor pullEventActor
        && pullAuthor /= pullEventActor
      ) mpEvents
