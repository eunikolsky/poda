{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module EventType where

import Data.Text (Text)
import Database.Persist.TH

data EventType
  -- draft/undraft: these events come from the `events` github endpoint
  = MarkDraft
  | MarkReady
  -- reviewed with result: these events come from the `timeline` github endpoint
  -- TODO it would be more semantically correct if these values were in their own ADT
  | Commented
  | Approved
  | DismissedApproval
  | RequestedChanges
  deriving (Show, Read, Eq)

derivePersistField "EventType"

mkEventType :: Text -> Maybe EventType
mkEventType "convert_to_draft" = Just MarkDraft
mkEventType "ready_for_review" = Just MarkReady
mkEventType "commented" = Just Commented
mkEventType "approved" = Just Approved
mkEventType "dismissed" = Just DismissedApproval
mkEventType "changes_requested" = Just RequestedChanges
mkEventType _ = Nothing
