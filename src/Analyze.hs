module Analyze
  ( DraftDurationInput(..)
  , MPull(..)
  , draftDuration
  , ourFirstReviewLatency
  ) where

import Control.Applicative ((<|>))
import Data.Maybe
import Data.Text (Text)
import Data.Time
import GHC.Stack (HasCallStack)

import Database
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
  }
  deriving Show

instance DraftDurationInput MPull where
  ddiCreated = pullCreated . mpPull
  ddiMerged = pullMerged . mpPull
  ddiEvents = mpEvents
  ddiIsDraft = pullIsDraft . mpPull

ourFirstReviewLatency :: [Text] -> MPull -> Maybe WorkDiffTime
ourFirstReviewLatency _ _ = Nothing
