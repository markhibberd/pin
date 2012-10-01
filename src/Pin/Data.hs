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


type PinCustomerToken = Text
type PinCardToken = Text
type PinChargeToken = Text
type PinName = Text
type PinCurrency = Maybe Text
type PinEmail = Text
type PinIp = Text
type PinMonth = Int
type PinYear = Int
type PinCardNumber = Text
type PinDisplayNumber = Text
type PinScheme = Text
type PinCvc = Int
type PinDate = Text

data PinChargeDetails =
  PinChargeDetails {
      pinChargeAmount :: PinAmount
    , pinChargeDescription :: Text
    , pinChargeCurrency :: Maybe Text
    }

data PinCard =
  PinCard PinCardNumber PinMonth PinYear PinCvc PinName PinAddress

data PinCharge =
    PinCharge PinCard PinChargeDetails PinEmail PinIp
  | PinChargeCard PinCardToken PinChargeDetails PinEmail PinIp
  | PinChargeCustomer PinCustomerToken PinChargeDetails PinEmail

data PinRefund =
    PinRefund PinChargeToken PinAmount

data PinCreateCard =
    PinCreateCard PinCard

data PinCreateCustomer =
    PinCreateCustomer PinEmail PinCard
  | PinCreateCustomerFromCard PinEmail PinCardToken

data PinDisplayCharge =
  PinDisplayCharge PinChargeToken Bool PinChargeDetails PinEmail PinIp PinDate (Maybe Text) (Maybe Text) PinDisplayCard -- FIX transfer????

data PinDisplayCard =
  PinDisplayCard PinCardToken PinDisplayNumber PinScheme PinAddress Bool

data PinCustomerResponse =
  PinCustomerResponse PinCustomerToken PinEmail PinDate PinDisplayCard

data PinResponse a =
    PinResponseSuccess a
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
