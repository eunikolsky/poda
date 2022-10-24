module Main where

import Control.Monad (forM_, unless)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Csv (encodeDefaultOrderedByName)
import Data.Time.Clock (getCurrentTime, utctDay)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as BL (writeFile)
import qualified Data.Text as T

import Database
import Lib

main :: IO ()
main = parseArgs >>= run

newtype Offline = Offline Bool
data Action = DropDerivedTables | Run Offline

parseArgs :: IO Action
parseArgs = do
  args <- getArgs
  pure $ case args of
    ["--drop-derived-tables"] -> DropDerivedTables
    ["--offline"] -> Run (Offline True)
    [] -> Run (Offline False)
    _ -> error "Unknown arguments"

run :: Action -> IO ()
run DropDerivedTables = dropDerivedTables
run (Run offline) = do
  config <- loadConfig
  pulls <- case offline of
    Offline False -> migrateDB >> listPRs config
    Offline True -> listLocalPRs
  let a = fmap (analyze config) pulls

  BL.writeFile "pulls.csv" $ encodeDefaultOrderedByName a
  let bySprint = groupBySprint (Sprint $ configFirstSprintStart config) a
  forM_ bySprint $ \sprint -> do
    saveSprintFile sprint
    printOpenTimes sprint

printOpenTimes :: (Sprint, [PullAnalysis]) -> IO ()
printOpenTimes (period, prs) = let prGroup = averageWorkOpenTime prs in do
  today <- utctDay <$> getCurrentTime
  putStrLn $ mconcat
    [ "Sprint ", show period
    , if inSprint period today then " (current sprint)" else ""
    , " had ", show $ prgPRCount prGroup, " PRs"
    , ", ", show $ prgMergedPRCount prGroup, " merged PRs"
    , "; average open time: ", maybe "N/A" (formatDiffTime . arOpenDuration) $ prgAverageResult prGroup
    , " (ignoring weekends: ", maybe "N/A" (formatDiffTime . arOpenWorkDuration) $ prgAverageResult prGroup, ")"
    , "; avg draft duration (ignoring weekends): ", maybe "0" formatDiffTime $ prgAverageWorkDraftDuration prGroup
    , "; avg latency of our first review (ignoring weekends): ", maybe "N/A" (formatDiffTime . grWorkLatency) $ prgAverageOurFirstReview prGroup
    , ", reviewers: ", maybe "none" (T.unpack . describeReviewActors . grActors) $ prgAverageOurFirstReview prGroup
    , "; avg latency of their first review (ignoring weekends): ", maybe "N/A" (formatDiffTime . grWorkLatency) $ prgAverageTheirFirstReview prGroup
    , ", reviewers: ", maybe "none" (T.unpack . describeReviewActors . grActors) $ prgAverageTheirFirstReview prGroup
    ]

saveSprintFile :: (Sprint, [PullAnalysis]) -> IO ()
saveSprintFile (sprint, prs) = do
  let outDir = "out"
  createDirectoryIfMissing False outDir
  BL.writeFile (outDir </> sprintFilename sprint <> ".csv") $ encodeDefaultOrderedByName prs

loadConfig :: IO Config
loadConfig = do
  let configFile = "config.json"
  unlessM (doesFileExist configFile) $ die "config.json not found"
  eitherConfig <- eitherDecodeFileStrict' configFile
  either (die . ("Couldn't load config.json: " ++)) pure eitherConfig

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb action = do
  b <- mb
  unless b action
