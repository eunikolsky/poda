{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}

module WorkDiffTime where

import Data.Csv (ToField, toField)
import Data.Time
import qualified Data.ByteString.Char8 as C

-- TODO remove orphan instances
-- | This allows to encode a @NominalDiffTime@ value into a CSV record.
instance ToField NominalDiffTime where
  toField = C.pack . show @Int . truncate @Double . realToFrac

-- | Represents a time duration calculated between two time points skipping
-- weekend days between them.
newtype WorkDiffTime = WorkDiffTime { unWorkDiffTime :: NominalDiffTime }
  deriving Eq

instance Show WorkDiffTime where
  show (WorkDiffTime dt) = mconcat ["WorkDiffTime ", show dt]

instance ToField WorkDiffTime where
  toField = toField . unWorkDiffTime

diffWorkTime :: UTCTime -> UTCTime -> WorkDiffTime
diffWorkTime to from = WorkDiffTime $ fullDiff - weekends
  where
    fromDay = utctDay from
    toDay = utctDay to

    fullDiff = diffUTCTime to from
    weekends = (* nominalDay) . fromIntegral . length . filter isWeekend . fmap dayOfWeek $ [fromDay..toDay]

    isWeekend Saturday = True
    isWeekend Sunday = True
    isWeekend _ = False

