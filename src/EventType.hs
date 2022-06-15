{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module EventType where

import Data.Text (Text)
import Database.Persist.TH

data EventType
  = MarkDraft
  | MarkReady
  deriving (Show, Read, Eq)

derivePersistField "EventType"

mkEventType :: Text -> Maybe EventType
mkEventType "convert_to_draft" = Just MarkDraft
mkEventType "ready_for_review" = Just MarkReady
mkEventType _ = Nothing
