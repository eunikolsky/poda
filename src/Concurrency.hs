module Concurrency
  ( MaxResources(..)

  , forConcurrentlyN
  ) where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent (newQSemN, signalQSemN, waitQSemN)
import Control.Exception (bracket_)

newtype MaxResources = MaxResources Int

-- | `Control.Concurrent.Async.forConcurrently` with limited concurrency — at most `MaxResources`
-- functions `f` can be run simultaneously.
forConcurrentlyN :: Traversable t => MaxResources -> t a -> (a -> IO b) -> IO (t b)
forConcurrentlyN (MaxResources maxResources) xs f = do
  resourceAvailable <- newQSemN maxResources

  forConcurrently xs $
    bracket_ (waitQSemN resourceAvailable 1) (signalQSemN resourceAvailable 1) . f
