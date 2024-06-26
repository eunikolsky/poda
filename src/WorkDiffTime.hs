{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WorkDiffTime (
  -- * Types
    UTCPeriod(..)
  , WorkDiffTime

  -- * Constructors
  , diffWorkTime

  -- * Getters
  , regular
  , work
  ) where

import Data.Monoid (Sum(..), getSum)
import Data.Time

-- | A period between two times, generally (bigger) @to@ and (smaller) @from@.
newtype UTCPeriod = UTCPeriod (UTCTime, UTCTime)
  deriving (Eq, Show)

-- | Represents a time duration calculated between two time points; can return
-- both the "regular" duration (including all days) and "work" duration
-- (excluding all weekends).
--
-- We store a list of periods because we have to know the actual dates and times
-- in order to filter out any weekends for "work time".
-- TODO rename to more generic
newtype WorkDiffTime = WorkDiffTime [UTCPeriod]
  deriving (Eq, Semigroup)

instance Show WorkDiffTime where
  show (WorkDiffTime dt) = mconcat ["WorkDiffTime ", show dt]

-- | Creates a `WorkDiffTime` from the @from@ and @to@ time points.
-- Use `<>` to combine multiple `WorkDiffTime`s.
diffWorkTime :: UTCTime -> UTCTime -> WorkDiffTime
diffWorkTime to = WorkDiffTime . pure . curry UTCPeriod to

-- | Returns the "regular" time difference, including all days.
regular :: WorkDiffTime -> NominalDiffTime
regular (WorkDiffTime periods) = getSum . foldMap (\(UTCPeriod (to, from)) -> Sum $ diffUTCTime to from) $ periods

-- | Returns the "work" time difference, that is excluding all weekends.
work :: WorkDiffTime -> NominalDiffTime
work (WorkDiffTime periods) = getSum . foldMap (Sum . workPeriod) $ periods

workPeriod :: UTCPeriod -> NominalDiffTime
workPeriod (UTCPeriod (to, from)) = fullDiff - weekends
  where
    fromDay = utctDay from
    toDay = utctDay to

    fullDiff = diffUTCTime to from
    -- TODO should we care about the local timezone even though the times are UTC?
    weekends = (* nominalDay) . fromIntegral . length . filter isWeekend . fmap dayOfWeek $ [fromDay..toDay]

    isWeekend Saturday = True
    isWeekend Sunday = True
    isWeekend _ = False
