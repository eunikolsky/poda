{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Conc where

import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent (threadDelay, newMVar, withMVar, newQSemN, signalQSemN, waitQSemN)
import Control.Exception (bracket_)
import Control.Monad
import Data.Time

newtype Input = Value Int

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

      worker :: Input -> IO ()
      worker (Value x) = do
          lPutStrLn . mconcat $ ["worker got input ", show x]
          threadDelay $ x * 1000000

  let maxResources = 4
  resourceAvailable <- newQSemN maxResources

  lPutStrLn "starting workers"
  forConcurrently_ input $
    bracket_ (waitQSemN resourceAvailable 1) (signalQSemN resourceAvailable 1) . worker

  lPutStrLn "done"
