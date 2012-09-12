{-# LANGUAGE OverloadedStrings #-}
module Pin.Core where

import Control.Monad
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import Data.Attoparsec.Lazy
import Data.Aeson
import Data.Text
import Data.Text.Encoding
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE

import Network.HTTP.Conduit
import Network.HTTP.Types

import Pin.Data

charge :: PinRequest -> IO PinResponse
charge req =
  parseUrl (unpack $ "https://api.pin.net.au/1/charges") >>= \url ->
  (liftM responder . withManager . httpLbs) (url {
      method = "GET" -- really? how dodgy are these guys?
    , requestHeaders = [
      ("Accept", "application/json")
    ]
    , checkStatus = const . const $ Nothing
  })

responder :: Response BL.ByteString -> PinResponse
responder = undefined
