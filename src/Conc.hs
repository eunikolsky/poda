{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Conc where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent (threadDelay, newMVar, withMVar, newQSemN, signalQSemN, waitQSemN)
import Control.Exception (bracket_)
import Control.Monad
import Data.Time

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

newtype MaxResources = MaxResources Int

-- | `Control.Concurrent.Async.forConcurrently` with limited concurrency — at most `MaxResources`
-- functions `f` can be run simultaneously.
forConcurrentlyN :: Traversable t => MaxResources -> t a -> (a -> IO b) -> IO (t b)
forConcurrentlyN (MaxResources maxResources) xs f = do
  resourceAvailable <- newQSemN maxResources

  forConcurrently xs $
    bracket_ (waitQSemN resourceAvailable 1) (signalQSemN resourceAvailable 1) . f
