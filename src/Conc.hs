{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Conc where

import Control.Concurrent (threadDelay, newMVar, withMVar)
import Control.Monad
import Data.Time

import Concurrency

newtype Input = Value Int

instance Show Input where
  show (Value i) = show i

conc :: IO ()
conc = do
  -- lock for `purStrLn` so that we don't have interleaved output
  -- https://stackoverflow.com/questions/2981984/can-i-ensure-that-haskell-performs-atomic-io
  lock <- newMVar ()

  -- task durations
  let input = take 10 . fmap Value $ cycle [1, 2, 3]

      lPutStrLn :: String -> IO ()
      lPutStrLn = withMVar lock . const . putStrLn <=< addTime
        where addTime s = (\t -> show (timeToTimeOfDay $ utctDayTime t) ++ ": " ++ s) <$> getCurrentTime

      worker :: Input -> IO Input
      worker i@(Value x) = do
        lPutStrLn . mconcat $ ["worker got input ", show x]
        threadDelay $ x * 1000000
        pure i

  lPutStrLn "starting workers"
  output <- forConcurrentlyN (MaxResources 4) input worker

  lPutStrLn $ "done; received: " <> show output
