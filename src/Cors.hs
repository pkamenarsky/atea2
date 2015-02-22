{-# LANGUAGE OverloadedStrings #-}
module Cors
    ( cors
    , addHeaders
    ) where

import Network.Wai
import Network.Wai.Internal
import Data.ByteString
import Network.HTTP.Types (Header)

cors :: Middleware
cors = addHeaders [("Access-Control-Allow-Origin", "*")]

addHeaders :: [Header] -> Middleware
addHeaders hs app req cb = do
  let fix rhs = rhs ++ hs
  app req $ \res -> cb $ case res of
    ResponseFile s rhs f mfp -> ResponseFile s (fix rhs) f mfp
    ResponseBuilder s rhs b -> ResponseBuilder s (fix rhs) b
    ResponseStream s rhs src -> ResponseStream s (fix rhs) src
    ResponseRaw f r -> ResponseRaw f r
