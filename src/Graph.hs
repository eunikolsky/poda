{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Graph
  ( plotOpenTimes
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
  . setFigureSize . texts . setTicks $ plot (enumFromTo 0 $ length times - 1) (snd <$> times)

  where
    logResult = either (hPutStrLn stderr) (\s -> unless (null s) $ putStrLn s)

    -- setting dpi doesn't work at all; setting figsize globally via `setParameter` doesn't work
    setFigureSize = (# "; fig.set(figwidth=10.24, figheight=7.68)")

    setTicks p = p # ";f=plot.gca();f.axes.xaxis.set_ticks(" # ticks # ", labels=[" # labels # "], rotation=10);f.axes.yaxis.set_ticklabels([])"
      where
        majorTicks = reverse . takeEvery 5 . reverse $ indexed times
        ticks = fst <$> majorTicks
        labels = fmap (ushow . show . fst . snd) majorTicks

    texts p = foldl'
      (\p' (idx, (_, dur)) -> p' # ";plot.text(" # idx # ", " # (floor $ realToFrac dur :: Int) # ", " # str (" " <> formatDiffTime dur) # ", ha='center', rotation='vertical')")
      p
      (indexed times)

indexed :: [a] -> [(Int, a)]
indexed = zip [0..]

takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery n (x:xs) = x : (takeEvery n . drop (n - 1) $ xs)
