{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Graph
  ( plotOpenTimes
  , plotPRCount
  ) where

import Control.Monad
import Data.Aeson
import Data.List
import Data.Time.Clock
import Graphics.Matplotlib
import System.IO
import Text.Show.Unicode

import Lib

plotGraph :: ToJSON a => (a -> Int) -> (a -> String) -> FilePath -> [SprintGrouped a] -> IO ()
plotGraph yvalfun labelfun filepath valuesBySprint = logResult <=< file filepath
  . setFigureSize . texts yvalfun labelfun values . setTicks valuesBySprint
  $ plot (enumFromTo 0 $ length valuesBySprint - 1) values
  where values = snd . fromSprintGrouped <$> valuesBySprint

plotOpenTimes :: FilePath -> [SprintGrouped NominalDiffTime] -> IO ()
plotOpenTimes = plotGraph (floor . realToFrac) formatDiffTime

plotPRCount :: FilePath -> [SprintGrouped Int] -> IO ()
plotPRCount = plotGraph id show

texts :: (a -> Int) -> (a -> String) -> [a] -> Matplotlib -> Matplotlib
texts yvalfun labelfun counts p = foldl'
  (\p' (idx, count) -> p' # ";plot.text(" # idx # ", " # yvalfun count # ", " # str (" " <> labelfun count) # ", ha='center', rotation='vertical')")
  p
  (indexed counts)

logResult :: Either String String -> IO ()
logResult = either (hPutStrLn stderr) (\s -> unless (null s) $ putStrLn s)

setTicks :: [SprintGrouped a] -> Matplotlib -> Matplotlib
setTicks counts p = p # ";f=plot.gca();f.axes.xaxis.set_ticks(" # ticks # ", labels=[" # labels # "], rotation=10);f.axes.yaxis.set_ticklabels([])"
  where
    majorTicks = reverse . takeEvery 5 . reverse . indexed . fmap fromSprintGrouped $ counts
    ticks = fst <$> majorTicks
    labels = fmap (ushow . show . fst . snd) majorTicks

setFigureSize :: Matplotlib -> Matplotlib
-- setting dpi doesn't work at all; setting figsize globally via `setParameter` doesn't work
setFigureSize = (# "; fig.set(figwidth=10.24, figheight=7.68)")

indexed :: [a] -> [(Int, a)]
indexed = zip [0..]

takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery n (x:xs) = x : (takeEvery n . drop (n - 1) $ xs)
