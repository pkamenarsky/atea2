{-# LANGUAGE DeriveGeneric, FlexibleContexts, RecordWildCards #-}

module Ticket where

import Control.Applicative ((*>))

import Data.List
import Data.Maybe

import GHC.Generics

import Text.Parsec
import Text.Parsec.Char

data TicketState = TSActive | TSInactive | TSDone deriving (Eq, Show, Generic)

data Ticket = Ticket
  { tckName     :: String
  , tckLevel    :: Int
  , tckState    :: TicketState
  , tckAssignee :: Maybe String
  , tckEstimate :: Maybe Int
  , tckChildren :: [Ticket]
  } deriving (Show, Generic)

prsLevel :: Stream s m Char => ParsecT s u m Int
prsLevel = length `fmap` (many1 $ char '*')

crlf :: (Stream s m Char) => ParsecT s u m Char
crlf                = char '\r' *> char '\n' <?> "crlf new-line"

endOfLine :: (Stream s m Char) => ParsecT s u m Char
endOfLine           = newline <|> crlf       <?> "new-line"

prsTicket :: Stream s m Char => ParsecT s u m Ticket
prsTicket = do
  tckLevel    <- prsLevel
  _           <- skipMany1 space
  state       <- optionMaybe $ try $ do
                   s <- choice
                     [ try $ string "WORKING"
                     , try $ string "WORK"
                     , try $ string "W"
                     , try $ string "IN-PROGRESS"
                     , try $ string "TODO"
                     , try $ string "DONE"
                     ]
                   _ <- skipMany1 space
                   return s
  tckName     <- many1 $ noneOf "\r\n"
  _           <- endOfLine

  let tckState = case state of
        Nothing            -> TSInactive
        Just "WORK"        -> TSActive
        Just "W"           -> TSActive
        Just "IN-PROGRESS" -> TSActive
        Just "TODO"        -> TSInactive
        Just "DONE"        -> TSDone

  let tckAssignee = Nothing
  let tckEstimate = Nothing
  let tckChildren = []

  return $ Ticket {..}

prsTickets :: Stream s m Char => ParsecT s u m [Ticket]
prsTickets = many1 prsTicket

orgTickets :: [Ticket] -> [Ticket]
orgTickets []     = []
orgTickets (t:ts) = t { tckChildren = orgTickets children } : orgTickets rs
    where
      (children, rs) = span ((> tckLevel t) . tckLevel) ts
