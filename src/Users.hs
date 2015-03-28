{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, OverloadedStrings, RecordWildCards, StandaloneDeriving, TypeOperators, TypeSynonymInstances #-}

module Users where

import           Data.Monoid
import qualified Data.Text        as T
import           Data.Proxy

import           Network.Wai.Handler.Warp

import           Servant.API
import           Servant.Server

type UserApi = "register" :> QueryParam "user" T.Text
                          :> QueryParam "pass" T.Text
                          :> QueryParam "email" T.Text
                          :> QueryParam "key" T.Text
                          :> Get T.Text

userApi :: Proxy UserApi
userApi = Proxy

userServer :: Server UserApi
userServer = apiRegister
  where
    apiRegister user' pass' email' key'
      | Just user  <- user'
      , Just pass  <- pass'
      , Just email <- email'
      , Just key   <- key'
        = return $ user <> pass <> email <> key
      | otherwise = return "error"

runUserServer :: IO ()
runUserServer = do
  run 8001 $ serve userApi userServer
