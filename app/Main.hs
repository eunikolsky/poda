module Main where

import Control.Monad (forM, unless)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Csv (encodeDefaultOrderedByName)
import Data.Time.Clock (getCurrentTime, utctDay)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import qualified Data.Bifunctor (second)
import qualified Data.ByteString.Lazy as BL (writeFile)
import qualified Data.Text as T
import qualified Data.Text.IO as T (writeFile)

import Asciidoc
import Database
import Graph
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

outDir :: FilePath
outDir = "out"

run :: Action -> IO ()
run DropDerivedTables = dropDerivedTables
run (Run offline) = do
  config <- loadConfig
  pulls <- case offline of
    Offline False -> migrateDB >> listPRs config
    Offline True -> listLocalPRs
  let a = fmap (analyze config) pulls

  createDirectoryIfMissing False outDir

  BL.writeFile (outDir </> "pulls.csv") $ encodeDefaultOrderedByName a
  let bySprint = reverse $ groupBySprint (Sprint $ configFirstSprintStart config) a
  today <- utctDay <$> getCurrentTime
  (reportTexts, openTimes) <- fmap untuples . forM bySprint $ \sprint -> do
    saveSprintFile sprint
    let prGroup = Data.Bifunctor.second averageWorkOpenTime sprint
    let openTime = Data.Bifunctor.second (maybe 0 arOpenWorkDuration . prgAverageResult) prGroup
    pure (sprintReport today prGroup, openTime)

  let imgFile = "opentimes.png"
  plotOpenTimes (outDir </> imgFile) $ reverse openTimes
  T.writeFile (outDir </> "report.adoc") . T.intercalate "\n\n" $
    [ reportHeader config today
    , reportImage (imgFile, "Average open times")
    , T.unlines reportTexts
    ]

untuples :: [(a, b)] -> ([a], [b])
untuples xs = (fst <$> xs, snd <$> xs)

saveSprintFile :: (Sprint, [PullAnalysis]) -> IO ()
saveSprintFile (sprint, prs) = do
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
