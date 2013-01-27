{-# LANGUAGE OverloadedStrings #-}
module Network.Api.Pin.Defaults where

import Data.Text

import Network.Api.Support
import Network.Api.Pin.Data
import Network.HTTP.Conduit


pinLiveConfig :: Text -> PinConfig
pinLiveConfig key =
  PinConfig "https://api.pin.net.au/1/charges" key def

pinTestConfig :: Text -> PinConfig
pinTestConfig key =
  PinConfig "https://test-api.pin.net.au/1/charges" key (def { managerCheckCerts = checkDomainOnly })
