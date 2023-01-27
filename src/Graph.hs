{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Graph
  ( plotOpenTimes
  , plotPRCount
  ) where

import Control.Monad
import Data.List
import Data.Time.Clock
import Graphics.Matplotlib
import System.IO
import Text.Show.Unicode

import Lib

plotOpenTimes :: FilePath -> [(Sprint, NominalDiffTime)] -> IO ()
plotOpenTimes filepath times = logResult <=< file filepath
  . setFigureSize . texts . setTicks times $ plot (enumFromTo 0 $ length times - 1) (snd <$> times)

  where
    texts p = foldl'
      (\p' (idx, (_, dur)) -> p' # ";plot.text(" # idx # ", " # (floor $ realToFrac dur :: Int) # ", " # str (" " <> formatDiffTime dur) # ", ha='center', rotation='vertical')")
      p
      (indexed times)

plotPRCount :: FilePath -> [(Sprint, Int)] -> IO ()
plotPRCount filepath counts = logResult <=< file filepath
  . setFigureSize . texts . setTicks counts $ plot (enumFromTo 0 $ length counts - 1) (snd <$> counts)

  where
    texts p = foldl'
      (\p' (idx, (_, count)) -> p' # ";plot.text(" # idx # ", " # count # ", " # str (" " <> show count) # ", ha='center', rotation='vertical')")
      p
      (indexed counts)

logResult :: Either String String -> IO ()
logResult = either (hPutStrLn stderr) (\s -> unless (null s) $ putStrLn s)

setTicks :: [(Sprint, a)] -> Matplotlib -> Matplotlib
setTicks counts p = p # ";f=plot.gca();f.axes.xaxis.set_ticks(" # ticks # ", labels=[" # labels # "], rotation=10);f.axes.yaxis.set_ticklabels([])"
  where
    majorTicks = reverse . takeEvery 5 . reverse $ indexed counts
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
