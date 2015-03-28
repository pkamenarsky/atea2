{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, RecordWildCards, StandaloneDeriving, TypeOperators, TypeSynonymInstances #-}

module Users where

import           Data.Monoid
import qualified Data.Text        as T
import           Data.Proxy

import           Network.Wai.Handler.Warp

import           Servant.API
import           Servant.Server

type UserApi = "register" :> Capture "user" T.Text
                          :> Capture "pass" T.Text
                          :> Capture "email" T.Text
                          :> Capture "key" T.Text
                          :> Get T.Text

userApi :: Proxy UserApi
userApi = Proxy

userServer :: Server UserApi
userServer = apiRegister
  where
    apiRegister user pass email key = return $ user <> pass <> email <> key

runUserServer :: IO ()
runUserServer = do
  run 8001 $ serve userApi userServer
