module Asciidoc
  ( reportHeader
  , reportImages
  , sprintReport
  ) where

import Data.List
import Data.Time.Calendar (Day)
import qualified Data.Text as T
import Text.Printf (printf)

import Lib

sprintReport :: Day -> (Sprint, PRGroup) -> T.Text
sprintReport today (period, prGroup) =
  T.unlines $ map (T.pack . concat)
    -- TODO use Builder
    [ [ "== Sprint *", show period, "*"
      , if inSprint period today then " (current)" else ""
      ]
    , [""]
    , [ "* ", show $ prgPRCount prGroup, " PRs, of them: "
      , show $ prgMergedPRCount prGroup, " merged PRs ("
      , if lowPercentMerged then "*" else ""
      , printf "%0.2f%%" percentMerged
      , if lowPercentMerged then "*" else ""
      , ")"
      ]
    , [ "* average open time: *", maybe "N/A" (formatDiffTime . arOpenWorkDuration) $ prgAverageResult prGroup, "*" ]
    , [ "* average _draft_ duration: *", maybe "0" formatDiffTime $ prgAverageWorkDraftDuration prGroup, "*" ]
    , [ "* average latency of _our_ first review: *"
      , maybe "N/A" (formatDiffTime . grWorkLatency) $ prgAverageOurFirstReview prGroup
      , "*, first reviewers: "
      , maybe "none" (T.unpack . describeReviewActors . grActors) $ prgAverageOurFirstReview prGroup
      ]
    , [ "* average latency of _their_ first review: *"
      , maybe "N/A" (formatDiffTime . grWorkLatency) $ prgAverageTheirFirstReview prGroup
      , "*, first reviewers: "
      , maybe "none" (T.unpack . describeReviewActors . grActors) $ prgAverageTheirFirstReview prGroup
      ]
    ]
  where
    percentMerged :: Double
    percentMerged = (fromIntegral (prgMergedPRCount prGroup) / fromIntegral (prgPRCount prGroup)) * 100.0
    lowPercentMerged = percentMerged <= 50

reportHeader :: Config -> Day -> T.Text
reportHeader config today = T.intercalate "\n\n" $ map (T.pack . concat)
  [ [ "= Pull Requests Report (", show today, ")" ]
  , [ "This report analyzed PRs by "
    , T.unpack . T.intercalate ", " . fmap (wrapIn "`") . sort $ configLocalTeam config
    , " (called _our_ reviewers below) in repositories: "
    , T.unpack . T.intercalate ", " . fmap (wrapIn "`" . unRepo) . sort $ configRepos config
    , ". All other reviewers are called _their_ reviewers below."
    ]
  , [ "*All times are work times (that is, ignoring weekends)!*" ]
  ]

reportImages :: [(FilePath, T.Text)] -> T.Text
reportImages images = T.intercalate "\n" $ map T.concat
  ( [ [ "== Graph" ] ]
  <> concatMap reportImage images
  )
  where
    reportImage :: (FilePath, T.Text) -> [[T.Text]]
    reportImage (file, title) =
      [ [ "" ]
      , [ ".", title ]
      , [ "image::", T.pack file, "[]" ]
      ]
