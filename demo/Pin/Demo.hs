{-# LANGUAGE OverloadedStrings #-}
module Pin.Demo where

import Pin
import Data.Text

demoCharge :: Text -> IO PinResponse
demoCharge key =
  charge key (PinRequest {
      pinAmount = 400
    , pinDescription = "test charge"
    , pinEmail = "roland@pin.net.au"
    , pinIp = "203.192.1.172"
    , pinNumber = "5520000000000000"
    , pinExpiryMonth = 05
    , pinExpiryYear = 2013
    , pinCvc = 519
    , pinName = "Roland Robot"
    , pinAddress = PinAddress {
        pinAddress1 = "42 Sevenoaks St"
      , pinAddress2 = Nothing
      , pinCity = "Lathlain"
      , pinPostcode = "6454"
      , pinState = "WA"
      , pinCountry = "AU"
      }
  })

demoFailure :: Text -> IO PinResponse
demoFailure key =
  charge key (PinRequest {
      pinAmount = 400
    , pinDescription = "test charge"
    , pinEmail = "roland@pin.net.au"
    , pinIp = "203.192.1.172"
    , pinNumber = "5560000000000001"
    , pinExpiryMonth = 05
    , pinExpiryYear = 2013
    , pinCvc = 519
    , pinName = "Roland Robot"
    , pinAddress = PinAddress {
        pinAddress1 = "42 Sevenoaks St"
      , pinAddress2 = Nothing
      , pinCity = "Lathlain"
      , pinPostcode = "6454"
      , pinState = "WA"
      , pinCountry = "AU"
      }
  })
