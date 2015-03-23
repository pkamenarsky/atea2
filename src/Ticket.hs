{-# LANGUAGE StandaloneDeriving, DeriveGeneric, FlexibleContexts, RecordWildCards, TypeFamilies, UndecidableInstances #-}

module Ticket where

import           Control.Arrow
import           Control.Applicative ((*>), (<$>))

import           Data.Hashable
import           Data.List
import           Data.Maybe
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T

import           GHC.Generics

import           Text.Parsec
import           Text.Parsec.Char

import           Git

data TicketState = TSActive | TSInactive | TSDone | TSArchived deriving (Eq, Show, Generic)

type Label = Int

data Parsed
data Org

data Diff a = Insert a | Delete a | Change a a | Move a a a deriving (Eq, Show)

data Ticket ty = Ticket
  { tckName     :: String
  , tckLevel    :: Int
  , tckLabel    :: Label
  , tckState    :: TicketState
  , tckAssignee :: Maybe String
  , tckEstimate :: Maybe Int
  , tckChildren :: [Label]
  } deriving (Show, Generic)

type ParsedTicket  = Ticket Parsed
type OrgTicket     = Ticket Org

prsLevel :: Stream s m Char => ParsecT s u m Int
prsLevel = length `fmap` (many1 $ char '*')

prsTicket :: Stream s m Char => ParsecT s u m ParsedTicket
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
  let tckLabel    = -1
  let tckChildren = []

  return $ Ticket {..}

prsTickets :: Stream s m Char => ParsecT s u m [ParsedTicket]
prsTickets = many prsTicket

data T a = T (Ticket a) [T a]

orgTickets :: Label -> [OrgTicket] -> [ParsedTicket] -> (Label, [OrgTicket])
orgTickets lbl ots pts = second (flat . org) $ lblTickets lbl ots pts
  where
    org []     = []
    org (t:ts) = T t (org children) : org rs
      where
        (children, rs) = span ((> tckLevel t) . tckLevel) ts

    flat [] = []
    flat ((T t ch):ts) = t { tckChildren = map (\(T t' _) -> tckLabel t') ch } : flat ch ++ flat ts

    -- TODO: do the diffing here
    lblTickets :: Label -> [OrgTicket] -> [ParsedTicket] -> (Label, [OrgTicket])
    lblTickets lbl _ [] = (lbl, [])
    lblTickets lbl lts (t:ts)
      | Just lt' <- lt = second (t { tckLabel = tckLabel lt' }:) $ lblTickets lbl lts ts
      | otherwise      = second (t { tckLabel = lbl + 1 }:) $ lblTickets (lbl + 1) lts ts
      where
        lt = find ((== tckName t) . tckName) lts

diffTickets :: Label -> [OrgTicket] -> [Content] -> (Label, [Diff OrgTicket])
diffTickets lbl ots = undefined
  where
    step :: Label -> [OrgTicket] -> Content -> Either ParseError (Label, [Diff OrgTicket])
    step lbl oldOts (Content {..}) = diff <$> newOts'
      where
        ohs     = M.fromList $ map (hash . tckName &&& id) oldOts
        pts     = runParser prsTickets () "" cntContent
        newOts' = orgTickets lbl oldOts <$> pts

        diff newOts = undefined
