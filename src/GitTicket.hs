{-# LANGUAGE RecordWildCards #-}

module GitTicket where

import           Control.Applicative

import           Text.Parsec  (runParser)

import           Git
import           Ticket

data GTState = GTState
  { stLabel       :: Label
  , stTickets     :: [OrgTicket]
  , stDiffs       :: [(Int, [Diff OrgTicket])]
  , stLastCommit  :: Maybe Commit
  }

updateTickets :: FilePath -> GTState -> IO GTState
updateTickets path st@(GTState {..}) = do
  cmts <- parseCommits <$> getCommits path (cmtHash <$> stLastCommit)
  cnts <- parseContents path "tasks.org" cmts

  return $ foldl updateState st (zip cmts cnts)

updateState :: GTState -> (Commit, Content) -> GTState
updateState (GTState {..}) (cmt, cnt)
  = GTState { stLabel       = label
            , stTickets     = tcks
            , stDiffs       = (cmtDate cmt, diff):stDiffs
            , stLastCommit  = Just cmt
            }
  where
    ptcks = either (const []) id $ runParser prsTickets () "" (cntContent cnt)
    (label, tcks, diff) = diffTickets stLabel stTickets ptcks
