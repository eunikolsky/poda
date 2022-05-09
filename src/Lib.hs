module Lib where

import System.Environment (getEnv)

githubToken :: IO String
githubToken = getEnv "GH_TOKEN"

someFunc :: IO ()
someFunc = putStrLn "someFunc"
