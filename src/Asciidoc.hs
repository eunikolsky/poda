module Asciidoc
  ( reportHeader
  , sprintReport
  ) where

import Data.List
import Data.Time.Calendar (Day)
import qualified Data.Text as T

import Lib

sprintReport :: Day -> (Sprint, [PullAnalysis]) -> T.Text
sprintReport today (period, prs) = let prGroup = averageWorkOpenTime prs in
  T.unlines $ map (T.pack . concat)
    -- TODO use Builder
    [ [ "== Sprint *", show period, "*"
      , if inSprint period today then " (current)" else ""
      ]
    , [""]
    , [ "* ", show $ prgPRCount prGroup, " PRs, of them: "
      , show $ prgMergedPRCount prGroup, " merged PRs"
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
  , [ "" ]
  ]
