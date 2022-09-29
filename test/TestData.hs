{-# LANGUAGE QuasiQuotes #-}

module TestData where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Text.RawString.QQ

-- | A redacted version of a real timeline events response github; the `user`
-- object has also been truncated.
timelineEventsString :: ByteString
timelineEventsString = BL.pack [r|
[
  {
    "sha": "123456",
    "node_id": "node",
    "url": "https://api.github.com/repos/proj/proj/git/commits/123456",
    "html_url": "https://github.com/proj/proj/commit/123456",
    "author": {
      "name": "john doe",
      "email": "johndoe@example.org",
      "date": "2022-09-06T16:50:00Z"
    },
    "committer": {
      "name": "john doe",
      "email": "johndoe@example.org",
      "date": "2022-09-06T16:50:00Z"
    },
    "tree": {
      "sha": "1290de",
      "url": "https://api.github.com/repos/proj/proj/git/trees/428e390fffa8571c578d5197e512721f47e4958c"
    },
    "message": "message",
    "parents": [
      {
        "sha": "9807",
        "url": "https://api.github.com/repos/proj/proj/git/commits/9980",
        "html_url": "https://github.com/proj/proj/commit/9980"
      }
    ],
    "verification": {
      "verified": false,
      "reason": "unsigned",
      "signature": null,
      "payload": null
    },
    "event": "committed"
  },
  {
    "id": 42,
    "node_id": "_",
    "user": {
      "login": "user",
      "id": 1
    },
    "body": "",
    "commit_id": "123456",
    "submitted_at": "2022-09-06T16:57:34Z",
    "state": "dismissed",
    "html_url": "https://github.com/proj/proj/pull/12#pullrequestreview-42",
    "pull_request_url": "https://api.github.com/repos/proj/proj/pulls/12",
    "author_association": "CONTRIBUTOR",
    "_links": {
      "html": {
        "href": "https://github.com/proj/proj/pull/12#pullrequestreview-42"
      },
      "pull_request": {
        "href": "https://api.github.com/repos/proj/proj/pulls/12"
      }
    },
    "event": "reviewed"
  },
  {
    "id": 100,
    "node_id": "_",
    "user": {
      "login": "user1",
      "id": 20
    },
    "body": "",
    "commit_id": "4567",
    "submitted_at": "2022-09-07T06:32:17Z",
    "state": "changes_requested",
    "html_url": "https://github.com/proj/proj/pull/12#pullrequestreview-100",
    "pull_request_url": "https://api.github.com/repos/proj/proj/pulls/12",
    "author_association": "COLLABORATOR",
    "_links": {
      "html": {
        "href": "https://github.com/proj/proj/pull/12#pullrequestreview-100"
      },
      "pull_request": {
        "href": "https://api.github.com/repos/proj/proj/pulls/12"
      }
    },
    "event": "reviewed"
  },
  {
    "id": 200,
    "node_id": "node",
    "user": {
      "login": "user2",
      "id": 21
    },
    "body": "",
    "commit_id": "678",
    "submitted_at": "2022-09-09T07:37:58Z",
    "state": "approved",
    "html_url": "https://github.com/proj/proj/pull/12#pullrequestreview-200",
    "pull_request_url": "https://api.github.com/repos/proj/proj/pulls/12",
    "author_association": "COLLABORATOR",
    "_links": {
      "html": {
        "href": "https://github.com/proj/proj/pull/12#pullrequestreview-200"
      },
      "pull_request": {
        "href": "https://api.github.com/repos/proj/proj/pulls/12"
      }
    },
    "event": "reviewed"
  },
  {
    "id": 333,
    "node_id": "_",
    "user": {
      "login": "user1",
      "id": 20
    },
    "body": "",
    "commit_id": "568",
    "submitted_at": "2022-09-09T23:06:31Z",
    "state": "commented",
    "html_url": "https://github.com/proj/proj/pull/12#pullrequestreview-333",
    "pull_request_url": "https://api.github.com/repos/proj/proj/pulls/12",
    "author_association": "COLLABORATOR",
    "_links": {
      "html": {
        "href": "https://github.com/proj/proj/pull/12#pullrequestreview-333"
      },
      "pull_request": {
        "href": "https://api.github.com/repos/proj/proj/pulls/12"
      }
    },
    "event": "reviewed"
  }
]
|]
