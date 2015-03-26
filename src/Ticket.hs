{-# LANGUAGE StandaloneDeriving, DeriveGeneric, FlexibleContexts, RecordWildCards #-}

module Ticket where

import           Control.Arrow
import           Control.Applicative              ((*>), (<$>))
import qualified Control.Monad.State              as ST

import           Data.Hashable
import           Data.List
import           Data.Maybe
import qualified Data.Map                         as M
import qualified Data.Set                         as S
import           Data.Serialize
import qualified Data.Text                        as T

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
  , tckParent   :: Maybe Label
  } deriving (Eq, Show, Generic)

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
  let tckParent   = Nothing

  return $ Ticket {..}

prsTickets :: Stream s m Char => ParsecT s u m [ParsedTicket]
prsTickets = many prsTicket

data T a = T (Ticket a) (Maybe (Ticket a)) [T a]

orgTickets :: Label -> [OrgTicket] -> [ParsedTicket] -> (Label, [OrgTicket])
orgTickets lbl ots pts = second (flat . org Nothing) $ lblTickets lbl ots pts
  where
    org _ []       = []
    org prn (t:ts) = T t prn (org (Just t) children) : org prn rs
      where
        (children, rs) = span ((> tckLevel t) . tckLevel) ts

    flat [] = []
    flat ((T t prn ch):ts) = t { tckParent = tckLabel <$> prn
                               , tckChildren = map (\(T t' _ _) -> tckLabel t') ch
                               } : flat ch ++ flat ts

    -- TODO: do the diffing here
    lblTickets :: Label -> [OrgTicket] -> [ParsedTicket] -> (Label, [OrgTicket])
    lblTickets lbl _ [] = (lbl, [])
    lblTickets lbl lts (t:ts)
      | Just lt' <- lt = second (t { tckLabel = tckLabel lt' }:) $ lblTickets lbl lts ts
      | otherwise      = second (t { tckLabel = lbl + 1 }:) $ lblTickets (lbl + 1) lts ts
      where
        lt = find ((== tckName t) . tckName) lts

diffTickets :: Label -> [OrgTicket] -> [ParsedTicket] -> (Label, [OrgTicket], [Diff OrgTicket])
diffTickets lbl old new = (lbl', newTckOrg, go oldTckMap newHashes)
  where
    oldTckMap          = M.fromList $ map (hash . tckName &&& id) old

    (lbl', newTckLbld) = label lbl oldTckMap $ map (hash . tckName &&& id) new
    newTckOrg          = flat $ org Nothing newTckLbld
    newHashes          = map (hash . tckName &&& id) newTckOrg

    org _ []       = []
    org prn (t:ts) = T t prn (org (Just t) children) : org prn rs
      where
        (children, rs) = span ((> tckLevel t) . tckLevel) ts

    flat [] = []
    flat ((T t prn ch):ts) = t { tckParent = tckLabel <$> prn
                               , tckChildren = map (\(T t' _ _) -> tckLabel t') ch
                               } : flat ch ++ flat ts

    label :: Label -> M.Map Int OrgTicket -> [(Int, ParsedTicket)] -> (Label, [OrgTicket])
    label lbl _ [] = (lbl, [])
    label lbl om ((thash, tck):ths)
      | Just otck <- M.lookup thash om
        = second (tck { tckLabel = tckLabel otck } :) $ label lbl om ths
      | otherwise
        = second (tck { tckLabel = lbl + 1 } :) $ label (lbl + 1) om ths

    go om [] = map Delete $ M.elems om
    go om ((thash, tck):ths)
      | Just otck <- M.lookup thash om = if otck == tck
        then go (M.delete thash om) ths
        else Change otck tck:go (M.delete thash om) ths
      | otherwise = Insert tck:go om ths

diffTest :: IO ()
diffTest = do
  f1 <- readFile "data/test.org"
  f2 <- readFile "data/test2.org"

  let t1 = either (const []) id $ runParser prsTickets () "" f1
      t2 = either (const []) id $ runParser prsTickets () "" f2

      (lbl, ots, diff) = diffTickets 0 [] t1
      (lbl2, ots2, diff2) = diffTickets lbl ots t2

  print diff2
