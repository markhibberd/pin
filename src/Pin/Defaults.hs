{-# LANGUAGE OverloadedStrings #-}
module Pin.Defaults where

import Data.Text

import Network.Api.Support
import Network.HTTP.Conduit

import Pin.Data

pinLiveConfig :: Text -> PinConfig
pinLiveConfig key =
  PinConfig "https://api.pin.net.au/1/charges" key def

pinTestConfig :: Text -> PinConfig
pinTestConfig key =
  PinConfig "https://test-api.pin.net.au/1/charges" key (def { managerCheckCerts = checkDomainOnly })

