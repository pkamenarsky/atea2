{-# LANGUAGE DataKinds, DeriveGeneric, RecordWildCards, StandaloneDeriving, TypeOperators #-}

module Main where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

import           Data.Aeson
import           Data.IORef
import qualified Data.Map                     as M
import           Data.Maybe
import           Data.Proxy

import           Network.Wai.Handler.Warp

import           System.Environment
import           System.FilePath.Posix
import qualified System.IO.Strict             as SIO

import           Servant.API
import           Servant.Client
import           Servant.Server

import           GHC.Generics

import           Logoot
import           Test

type Ack = Int

data ReqPrintState

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

data AppState = AppState
  { asClock :: Clock
  , asText  :: LString
  , asAck   :: M.Map Site Ack
  , asOps   :: [((Site, Ack), [Op])]
  } deriving (Show)

type AppStateIO = IORef AppState

instance FromJSON ReqPush
instance ToJSON ReqPush

instance FromJSON ReqPull
instance ToJSON ReqPull

instance FromJSON ResPull
instance ToJSON ResPull

deriving instance Generic Op
instance FromJSON Op
instance ToJSON Op

type Api = "push" :> ReqBody ReqPush :> Post ()
      :<|> "pull" :> ReqBody ReqPull :> Post ResPull
      :<|> "create" :> Get Clock
      :<|> "print" :> Get ()

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
  where
    rPush (ReqPush {..}) = liftIO $ do
      as <- readIORef st
      let txt'    = integrate rqpOps (asText as)
          lastAck = fromMaybe 0 $ (snd . fst) <$> (lastMay $ asOps as)
      writeIORef st $ as { asText = txt'
                         , asOps  = asOps as ++ [((rqprSite, lastAck + 1), rqpOps)] -- reverse?
                         }

    rPull (ReqPull {..}) = liftIO $ do
      as <- readIORef st
      let siteAck = fromMaybe (-1) $ M.lookup rqpSite (asAck as)
          lastAck = fromMaybe (rqpSite, 0) $ fst <$> (lastMay $ asOps as)
      writeIORef st $ as { asAck = M.insert rqpSite (snd lastAck) (asAck as) }
      -- print $ ResPull $ concatMap snd $ filter ((> siteAck) . fst) (asOps as)
      return $ ResPull $ concatMap snd $ filter (\((site, ack), _) -> (ack > siteAck) && site /= rqpSite) (asOps as)

    rCreate = liftIO $ do
      as <- readIORef st
      writeIORef st $ as { asClock = incSite $ asClock as }
      return $ asClock as

    rPrint = liftIO $ do
      as <- readIORef st
      print as

runServer :: IO ()
runServer = do
  st <- newIORef $ AppState (0, 1) emptyLString M.empty []
  run 8000 $ serve api $ server st

(reqPush :<|> reqPull :<|> reqCreate :<|> reqPrint) = client api

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
