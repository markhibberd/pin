{-# LANGUAGE OverloadedStrings #-}
module Pin.Data where

import Control.Applicative

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import Data.Vector (toList)

import Network.HTTP.Conduit

toParams :: PinRequest -> [(B.ByteString, B.ByteString)]
toParams pin = [
      ("amount", fromInt . pinAmount $ pin)
    , ("description", fromText . pinDescription $ pin)
    , ("email", fromText . pinEmail $ pin)
    , ("ip_address", fromText . pinIp $ pin)
    , ("card[number]", fromText . pinNumber $ pin)
    , ("card[expiry_month]", fromInt . pinExpiryMonth $ pin)
    , ("card[expiry_year]", fromInt . pinExpiryYear $ pin)
    , ("card[cvc]", fromInt . pinCvc $ pin)
    , ("card[name]", fromText . pinName $ pin)
    , ("card[address_line1]", fromText . pinAddress1 . pinAddress $ pin)
    , ("card[address_line2]", fromText . fromMaybe "" . pinAddress2 . pinAddress $ pin)
    , ("card[address_city]", fromText . pinCity . pinAddress $ pin)
    , ("card[address_postcode]", fromText . pinPostcode . pinAddress $ pin)
    , ("card[address_state]", fromText . pinState . pinAddress $ pin)
    , ("card[address_country]", fromText . pinCountry . pinAddress $ pin)
    ]

fromInt :: Int -> B.ByteString
fromInt = fromText . pack . show

fromText :: Text -> B.ByteString
fromText = encodeUtf8

toText :: BL.ByteString -> Text
toText = LT.toStrict . LE.decodeUtf8

toStrictBS :: BL.ByteString -> B.ByteString
toStrictBS = B.concat . BL.toChunks

-- urlEncodedBody params

type PinAmount = Int -- Amount in cents

data PinConfig = PinConfig {
    pinUrl :: Text
  , pinApiKey :: Text
  , pinManagerSettings :: ManagerSettings
  }

pinApiKeyBS :: PinConfig -> B.ByteString
pinApiKeyBS = encodeUtf8 . pinApiKey

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
