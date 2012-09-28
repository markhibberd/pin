{-# LANGUAGE OverloadedStrings #-}
module Pin.Data where

import Control.Applicative
import Data.Aeson
import Data.Text

{--
Request

curl https://api.pin.net.au/1/charges \
-u your-api-key: \
-d "amount=400" \
-d "description=test charge" \
-d "email=roland@pin.net.au" \
-d "ip_address=203.192.1.172" \
-d "card[number]=5123456789012346" \
-d "card[expiry_month]=05" \
-d "card[expiry_year]=2013" \
-d "card[cvc]=519" \
-d "card[name]=Roland Robot" \
-d "card[address_line1]=42 Sevenoaks St" \
-d "card[address_city]=Lathlain" \
-d "card[address_postcode]=6454" \
-d "card[address_state]=WA" \
-d "card[address_country]=AU"

--}

type PinAmount = Int -- Amount in cents

data PinAddress =
  PinAddress {
      pinAddress1 :: Text
    , pinAddress2 :: Maybe Text
    , pinCity :: Text
    , pinPostcode :: Text
    , pinState :: Text
    , pinCountry :: Text
    }
  deriving (Show)

data PinRequest =
  PinRequest {
      pinAmount :: PinAmount
    , pinDescription :: Text
    , pinEmail :: Text
    , pinIp :: Text
    , pinNumber :: Text
    , pinExpiryMonth :: Int
    , pinExpiryYear :: Int
    , pinCvc :: Int
    , pinName :: Text
    , pinAddress :: PinAddress
    }

{--
Response

{
  "response":{
    "token":"ch__Eqppblg_DAbmdBYxUigfg",
    "success":true,
    "amount":400,
    "description":"test charge",
    "email":"roland@pin.net.au",
    "ip_address":"203.192.1.172",
    "created_at":"2012-05-23T06:07:42Z",
    "card":{
      "token":"tok_vtcb7jewpBGvIQQvJohPTg",
      "display_number":"XXXX-XXXX-XXXX-2346",
      "scheme":"master",
      "address_line1":"42 Sevenoaks St",
      "address_line2":"",
      "address_city":"Lathlain",
      "address_postcode":"6454",
      "address_state":"WA",
      "address_country":"AU"
    }
  }
}

--}

data PinResponse =
    PinResponseSuccess {
        pinResponseToken :: Text
      , pinResponseResult :: Bool
      , pinResponseAmount :: PinAmount
      , pinResponseDescription :: Text
      , pinResponseEmail :: Text
      , pinResponseIp :: Text
      , pinResponseTimestamp :: Text
      , pinResponseCard :: PinCard
      }
  | PinResponseUnauthorized
  | PinResponseClientError Text
  | PinResponseServerError Text
  | PinResponseInvalidResponseCode Int Text
  | PinResponseJsonSyntaxError Int Text Text
  | PinResponseJsonFormatError Int Text Text
  deriving (Show)

data PinResponseSuccessData =
  PinResponseSuccessData
    Text
    Bool
    PinAmount
    Text
    Text
    Text
    Text
    PinCard

data PinCard =
  PinCard {
      pinCardToken :: Text
    , pinCardDisplayNumber :: Text
    , pinCardScheme :: Text
    , pinCardAddress :: PinAddress
    }
  deriving (Show)

instance FromJSON PinResponseSuccessData where
  parseJSON (Object o) =
    o .: "response" >>= \response -> case response of
      (Object oo) -> PinResponseSuccessData
        <$> oo .: "token"
        <*> oo .: "success"
        <*> oo .: "amount"
        <*> oo .: "description"
        <*> oo .: "email"
        <*> oo .: "ip_address"
        <*> oo .: "created_at"
        <*> parseJSON response
      _ -> fail "Invalid PinCard.response"
  parseJSON _ = fail "Invalid PinCard"


instance FromJSON PinAddress where
  parseJSON (Object o) = PinAddress
    <$> o .: "address_line1"
    <*> o .: "address_line2"
    <*> o .: "address_city"
    <*> o .: "address_postcode"
    <*> o .: "address_state"
    <*> o .: "address_country"
  parseJSON _ = fail "Invalid PinCard"


instance FromJSON PinCard where
  parseJSON o'@(Object o) = PinCard
    <$> o .: "token"
    <*> o .: "display_number"
    <*> o .: "scheme"
    <*> parseJSON o'
  parseJSON _ = fail "Invalid PinCard"
