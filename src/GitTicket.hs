{-# LANGUAGE RecordWildCards #-}

module GitTicket where

import           Text.Parsec  (runParser)

import           Git
import           Ticket

data GTState = GTState
  { stLabel   :: Label
  , stTickets :: [OrgTicket]
  , stDiffs   :: [(Int, [Diff OrgTicket])]
  }

updateTickets :: GTState -> IO GTState
updateTickets st = do
  last <- getLastParsedCommit
  cmts <- getCommits last

  let parsed = parseCommits cmts

  cnts <- parseContents "tasks.org" parsed

  case parsed of
    []  -> return ()
    x:_ -> setLastParsedCommit $ cmtHash x

  return $ foldl updateState st (zip parsed cnts)

updateState :: GTState -> (Commit, Content) -> GTState
updateState (GTState {..}) (cmt, cnt)
  = GTState { stLabel   = label
            , stTickets = tcks
            , stDiffs   = (cmtDate cmt, diff):stDiffs
            }
  where
    ptcks = either (const []) id $ runParser prsTickets () "" (cntContent cnt)
    (label, tcks, diff) = diffTickets stLabel stTickets ptcks
