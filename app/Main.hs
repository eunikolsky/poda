module Main where

import Control.Monad (forM_, unless)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Csv (encodeDefaultOrderedByName)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (die)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as BL (writeFile)

import Lib

main :: IO ()
main = do
  config <- loadConfig
  migrateDB
  a <- analyzePRs <$> listPRs config

  BL.writeFile "pulls.csv" $ encodeDefaultOrderedByName a
  saveSprintFiles config a
  printOpenTimes a

printOpenTimes :: [PullAnalysis] -> IO ()
printOpenTimes a =
  forM_ (averageWorkOpenTimeByMonth a) $ \(yearMonth, prGroup) ->
    putStrLn $ mconcat
      [ show yearMonth
      , " had ", show $ prgPRCount prGroup, " PRs"
      , ", ", show $ prgMergedPRCount prGroup, " merged PRs"
      , "; average open time: ", maybe "N/A" (formatDiffTime . arOpenDuration) $ prgAverageResult prGroup
      , " (ignoring weekends: ", maybe "N/A" (formatDiffTime . unWorkDiffTime . arOpenWorkDuration) $ prgAverageResult prGroup, ")"
      ]

saveSprintFiles :: Config -> [PullAnalysis] -> IO ()
saveSprintFiles (Config { configFirstSprintStart = firstSprintStart }) a = do
  let outDir = "out"
  createDirectoryIfMissing False outDir
  forM_ (groupBySprint (Sprint firstSprintStart) a) $ \(sprint, prs) ->
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
