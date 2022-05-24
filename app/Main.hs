module Main where

import Data.Csv (encodeDefaultOrderedByName)
import qualified Data.ByteString.Lazy as BL (writeFile)
import Lib

main :: IO ()
main = do
  a <- analyzePRs <$> listPRs
  BL.writeFile "pulls.csv" $ encodeDefaultOrderedByName a
