{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, RecordWildCards, StandaloneDeriving, TypeOperators, TypeSynonymInstances #-}

module Main where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

import           Data.Aeson
import           Data.IORef
import           Data.List
import qualified Data.Map                     as M
import           Data.Maybe
import           Data.Proxy
import           Data.Time.Clock

import           Text.Parsec

import           Network.Wai.Handler.Warp

import           System.Environment
import           System.FilePath.Posix
import qualified System.IO.Strict             as SIO

import           Servant.API
import           Servant.Client
import           Servant.Server

import           GHC.Generics

import           Cors
import           Diff
import           DVCS
import           Git
import           Logoot
import           Test
import           Ticket

type Ack = Int

data ReqPrintState

data ReqTickets

data ReqPush = ReqPush
  { rqpOps   :: [Op]
  , rqprSite  :: Site
  } deriving (Show, Generic)

data ReqPull = ReqPull
  { rqpSite :: Site
  } deriving (Show, Generic)

data ResPull = ResPull
  { rspOps :: [Op]
  } deriving (Show, Generic)

data ResTickets = ResTickets
  { rsActiveTicket :: Maybe (Label, UTCTime)
  , rsTickets     :: [OrgTicket]
  , rsTimeLog     :: [(Label, UTCTime, NominalDiffTime)]
  } deriving (Show, Generic)

data AppState = AppState
  { asClock        :: Clock
  , asLabel        :: Label
  , asText         :: LString
  , asAck          :: M.Map Site Ack
  , asOps          :: [((Site, Ack), [Op])]
  , asActiveTicket :: Maybe (Label, UTCTime)
  , asTickets      :: [OrgTicket]
  , asTimeLog      :: [(Label, UTCTime, NominalDiffTime)]
  } deriving (Show)

type AppStateIO = IORef AppState

instance FromJSON ReqPush
instance ToJSON ReqPush

instance FromJSON ReqPull
instance ToJSON ReqPull

instance FromJSON ResPull
instance ToJSON ResPull

instance FromJSON ResTickets
instance ToJSON ResTickets

r2i :: Rational -> Int
r2i = floor

i2r :: Int -> Rational
i2r = fromIntegral

instance ToJSON NominalDiffTime where
    toJSON  = toJSON . r2i . toRational

instance FromJSON NominalDiffTime where
    parseJSON v = fromRational . i2r <$> parseJSON v

deriving instance Generic Op
instance FromJSON Op
instance ToJSON Op

instance FromJSON TicketState
instance ToJSON TicketState

instance FromJSON ParsedTicket
instance ToJSON ParsedTicket

instance FromJSON OrgTicket
instance ToJSON OrgTicket

type Api = "push"    :> ReqBody ReqPush :> Post ()
      :<|> "pull"    :> ReqBody ReqPull :> Post ResPull
      :<|> "create"  :> Get Clock
      :<|> "print"   :> Get ()
      :<|> "tickets" :> Get ResTickets

api :: Proxy Api
api = Proxy

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay xs = Just $ last xs

server :: AppStateIO -> Server Api
server st = rPush
       :<|> rPull
       :<|> rCreate
       :<|> rPrint
       :<|> rTickets
  where
    rPush (ReqPush {..}) = liftIO $ do
      putStrLn $ "Receiving push from " ++ show rqprSite

      as <- readIORef st

      let text      = integrate rqpOps (asText as)
          tickets   = either (const []) id $ runParser prsTickets () "" $ showLString text
          lastAck   = fromMaybe 0 $ (snd . fst) <$> (lastMay $ asOps as)
          (label , tickets')
                    = orgTickets (asLabel as) (asTickets as) tickets
          activeNow = find ((== TSActive) . tckState) tickets'

      -- print $ runParser prsTickets () "" $ showLString text

      time <- getCurrentTime

      let (activeTicket, timeLog)
            | Just t             <- activeNow
            , Just (lbl, since)  <- asActiveTicket as = if tckLabel t == lbl
                then (asActiveTicket as, asTimeLog as)
                else (Just (tckLabel t, time) , (lbl, since, time `diffUTCTime` since):asTimeLog as)
            | Just t  <- activeNow = (Just (tckLabel t, time), asTimeLog as)
            | Just (lbl, since)  <- asActiveTicket as = (Nothing, (lbl, since, time `diffUTCTime` since):asTimeLog as)
            | otherwise = (Nothing, asTimeLog as)

      writeIORef st $ as
        { asLabel        = label
        , asText         = text
        , asOps          = asOps as ++ [((rqprSite, lastAck + 1), rqpOps)] -- reverse?
        , asActiveTicket = activeTicket
        , asTickets      = tickets'
        , asTimeLog      = timeLog
        }

    rPull (ReqPull {..}) = liftIO $ do
      putStrLn $ "Receiving pull from " ++ show rqpSite

      as <- readIORef st
      let siteAck = fromMaybe (-1) $ M.lookup rqpSite (asAck as)
          lastAck = fromMaybe (rqpSite, 0) $ fst <$> (lastMay $ asOps as)
      writeIORef st $ as { asAck = M.insert rqpSite (snd lastAck) (asAck as) }
      -- print $ ResPull $ concatMap snd $ filter ((> siteAck) . fst) (asOps as)
      return $ ResPull $ concatMap snd $ filter (\((site, ack), _) -> (ack > siteAck) && site /= rqpSite) (asOps as)

    rCreate = liftIO $ do
      as <- readIORef st
      writeIORef st $ as { asClock = incSite $ asClock as }

      putStrLn $ "Receiving create, new site: " ++ show (incSite $ asClock as)

      return $ asClock as

    rPrint = liftIO $ do
      putStrLn "Receiving print state"

      as <- readIORef st
      print as

    rTickets = liftIO $ do
      as <- readIORef st

      return $ ResTickets
        { rsActiveTicket = asActiveTicket as
        , rsTickets      = asTickets as
        , rsTimeLog      = asTimeLog as
        }

runServer :: IO ()
runServer = do
  st <- newIORef $ AppState (0, 1) 0 emptyLString M.empty [] Nothing [] []
  run 8000 {-- $ cors --} $ serve api $ server st

(reqPush :<|> reqPull :<|> reqCreate :<|> reqPrint :<|> reqTickets) = client api

--

Right url = parseBaseUrl "http://localhost:8000"

argCreate :: String -> IO ()
argCreate file = do
  Right cl <- runEitherT $ reqCreate url

  writeFile (file ++ ".org") ""
  writeFile (file ++ ".atea") $ show $ emptyLFile cl

argPush :: String -> IO ()
argPush file = do
  str        <- readFile (file ++ ".org")
  (lstr, cl) <- read <$> readFile (file ++ ".atea")

  putStrLn "Integrating..."

  let (ops, cl') = diffLString cl str lstr
  let lstr' = integrate ops lstr

  writeFile (file ++ ".atea") $ show $ (lstr', cl')
  putStrLn "Sending changes..."

  _ <- runEitherT $ reqPush (ReqPush ops (clkSite cl)) url
  return ()

argPull :: String -> IO ()
argPull file = do
  str        <- readFile (file ++ ".org")
  (lstr, cl) <- read <$> readFile (file ++ ".atea")

  putStrLn "Requesting changes..."
  Right (ResPull {..})
             <- runEitherT $ reqPull (ReqPull $ clkSite cl) url

  putStrLn "Integrating..."
  let lstr' = integrate rspOps lstr

  writeFile (file ++ ".org") (showLString lstr')
  writeFile (file ++ ".atea") $ show (lstr', cl)

argPrint :: IO (Either String ())
argPrint = runEitherT $ reqPrint url

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["--serve"]       -> runServer
    "--create":file:_ -> argCreate $ dropExtension file
    "--push":file:_   -> argPush $ dropExtension file
    "--pull":file:_   -> argPull $ dropExtension file
    otherwise         -> return ()

test :: IO ()
test = do
  argCreate "test"
  argCreate "test2"

  writeFile "test.org" "666\n"
  writeFile "test2.org" "777\n"

  argPush "test"
  argPush "test2"

  argPull "test"
  argPull "test2"

  writeFile "test.org" "68866\n777\n"
  writeFile "test2.org" "777\n"

  argPush "test"
  argPush "test2"

  argPull "test"
  argPull "test2"
