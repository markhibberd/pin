{-# LANGUAGE OverloadedStrings #-}
module Pin.Data where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Data.Vector (toList)

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
  | PinResponseUnproccessible {
        pinResponseError :: Text
      , pinResponseErrorDescription :: Text
      , pinResponseMessages :: [(Text, Text)] -- (Code, Message)
      }
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

data PinResponseUnproccessibleData =
  PinResponseUnproccessibleData
    Text
    Text
    [(Text, Text)]

data PinCard =
  PinCard {
      pinCardToken :: Text
    , pinCardDisplayNumber :: Text
    , pinCardScheme :: Text
    , pinCardAddress :: PinAddress
    }
  deriving (Show)

message :: Value -> Parser (Text, Text)
message (Object o) = (,) <$> o .: "code" <*> o .: "message"
message _ = fail "Expected message to be an object"

messages :: Value -> Parser [(Text, Text)]
messages (Array a) = mapM message (toList a)
messages _ = fail "Expected messages to be an array"

instance FromJSON PinResponseUnproccessibleData where
  parseJSON (Object o) = PinResponseUnproccessibleData
    <$> o .: "error"
    <*> o .: "error_description"
    <*> (o .: "messages" >>= messages)
  parseJSON _ = fail "Invalid PinResponseUnproccessibleData"

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
        <*> oo .: "card"
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
