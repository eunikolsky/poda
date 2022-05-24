module Main where

import Control.Monad (unless)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Csv (encodeDefaultOrderedByName)
import System.Directory (doesFileExist)
import System.Exit (die)
import qualified Data.ByteString.Lazy as BL (writeFile)

import Lib

main :: IO ()
main = do
  config <- loadConfig
  migrateDB
  a <- analyzePRs <$> listPRs config
  BL.writeFile "pulls.csv" $ encodeDefaultOrderedByName a

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
