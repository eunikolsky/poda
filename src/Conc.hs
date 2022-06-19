{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Conc where

import Control.Concurrent.Chan
import Control.Concurrent (threadDelay, forkIO, newMVar, withMVar, newQSemN, signalQSemN, waitQSemN)
import Control.Monad
import Data.Time

data Input = Value Int | End

conc :: IO ()
conc = do
  -- https://stackoverflow.com/questions/9616515/are-haskell-channels-control-concurrent-chan-safe-for-multiple-readers-produce
  chan <- newChan
  endCounter <- newQSemN 0

  -- lock for `purStrLn` so that we don't have interleaved output
  -- https://stackoverflow.com/questions/2981984/can-i-ensure-that-haskell-performs-atomic-io
  lock <- newMVar ()

  -- task durations
  let input = take 10 . fmap Value $ cycle [1, 2, 3]
      numWorkers = 4

      lPutStrLn :: String -> IO ()
      lPutStrLn = withMVar lock . const . putStrLn <=< addTime
        where addTime s = (\t -> show (timeToTimeOfDay $ utctDayTime t) ++ ": " ++ s) <$> getCurrentTime

      worker n = do
        lPutStrLn . mconcat $ ["worker #", show n, " waiting for input"]
        x <- readChan chan
        case x of
          Value x -> do
            lPutStrLn . mconcat $ ["worker #", show n, " got input ", show x]
            threadDelay $ x * 1000000
            worker n

          End -> do
            lPutStrLn . mconcat $ ["worker #", show n, " terminating"]
            signalQSemN endCounter 1

  lPutStrLn "starting workers"
  forM_ [0 :: Int .. numWorkers-1] $ forkIO . worker

  lPutStrLn "sending input"
  forM_ input $ writeChan chan
  forM_ [0 :: Int .. numWorkers-1] . const $ writeChan chan End

  lPutStrLn "waiting for workers to finish"
  waitQSemN endCounter numWorkers
  lPutStrLn "done"
