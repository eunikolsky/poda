{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getEnv)
import qualified Data.ByteString.Char8 as C

githubToken :: IO String
githubToken = getEnv "GH_TOKEN"

-- | Contains `owner/repo`.
newtype Repo = Repo String
  deriving Show

data PR = PR
  { id :: Int
  , title :: Text
  , url :: Text
  }
  deriving (Generic, Show)

instance FromJSON PR

listPRs :: Repo -> IO ()
listPRs (Repo repo) = do
  manager <- newManager tlsManagerSettings
  request <- githubRequest . APIPath . mconcat $ ["/repos/", repo, "/pulls?per_page=5"]
  response <- httpLbs request manager

  let prs :: [PR] = fromMaybe [] . decode $ responseBody response
  mapM_ print prs

  print $ responseHeaders response

newtype APIPath = APIPath String
  deriving Show

githubRequest :: APIPath -> IO Request
githubRequest (APIPath apiPath) = do
  token <- githubToken
  request <- parseRequest $ mconcat ["https://api.github.com", apiPath]
  pure $ request { requestHeaders =
    [ ("accept", "application/vnd.github.v3+json")
    , ("authorization", mconcat ["token ", C.pack token])
    , ("user-agent", "eunikolsky/poda")
    ] }

someFunc :: IO ()
someFunc = putStrLn "someFunc"
