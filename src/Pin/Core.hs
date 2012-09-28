{-# LANGUAGE OverloadedStrings #-}
module Pin.Core (charge) where

import Control.Monad

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Attoparsec.Lazy
import Data.Aeson
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE

import Network.HTTP.Conduit
import Network.HTTP.Types

import Pin.Data
import Pin.Network

charge :: Text -> PinRequest -> IO PinResponse
charge key req =
  parseUrl (unpack $ "https://test-api.pin.net.au/1/charges") >>= \url ->
  (liftM responder . withCustomManager (def { managerCheckCerts = checkDomainOnly } ) . httpLbs) (applyBasicAuth (fromText key) "" $ urlEncodedBody (params req) $ url {
      method = "POST"
    , requestHeaders = [
      ("Accept", "application/json")
    ]
    , checkStatus = const . const $ Nothing
  })

params :: PinRequest -> [(B.ByteString, B.ByteString)]
params r = [
    ("amount", fromInt . pinAmount $ r)
  , ("description", fromText . pinDescription $ r)
  , ("email", fromText . pinEmail $ r)
  , ("ip_address", fromText . pinIp $ r)
  , ("card[number]", fromText . pinNumber $ r)
  , ("card[expiry_month]", fromInt . pinExpiryMonth $ r)
  , ("card[expiry_year]", fromInt . pinExpiryYear $ r)
  , ("card[cvc]", fromInt . pinCvc $ r)
  , ("card[name]", fromText . pinName $ r)
  , ("card[address_line1]", fromText . pinAddress1 . pinAddress $ r)
  , ("card[address_line2]", fromText . fromMaybe "" . pinAddress2 . pinAddress $ r)
  , ("card[address_city]", fromText . pinCity . pinAddress $ r)
  , ("card[address_postcode]", fromText . pinPostcode . pinAddress $ r)
  , ("card[address_state]", fromText . pinState . pinAddress $ r)
  , ("card[address_country]", fromText . pinCountry . pinAddress $ r)
  ]

fromInt :: Int -> B.ByteString
fromInt = fromText . pack . show

fromText :: Text -> B.ByteString
fromText = encodeUtf8

toText :: BL.ByteString -> Text
toText = LT.toStrict . LE.decodeUtf8

toStrictBS :: BL.ByteString -> B.ByteString
toStrictBS = B.concat . BL.toChunks

withJson :: FromJSON a => Int -> BL.ByteString -> (a -> PinResponse) -> PinResponse
withJson code bs f =
  let bt = toText bs
  in case parseJson bs of
    Left (Left msg) -> PinResponseJsonSyntaxError code msg bt
    Left (Right msg) -> PinResponseJsonFormatError code msg bt
    Right a -> f a

parseJson :: FromJSON a => BL.ByteString -> Either (Either Text Text) a
parseJson bs =
  case parseOnly json (toStrictBS bs) of
    Left msg -> Left (Left . pack $ msg)
    Right j -> case fromJSON j of
      (Error msg') -> Left (Right . pack $ msg')
      (Success a) -> Right a

responder :: Response BL.ByteString -> PinResponse
responder (Response status _ _ body) =
  let bt = toText body
   in case status of
    (Status 200 _) ->
      withJson 200 body (\(PinResponseSuccessData t r a d e ip ts c) -> PinResponseSuccess t r a d e ip ts c)
    (Status 401 _) ->
      PinResponseUnauthorized
    (Status 422 _) ->
      withJson 422 body (\(PinResponseUnproccessibleData e d ms) -> PinResponseUnproccessible e d ms)
    (Status 500 _) ->
      PinResponseServerError bt
    (Status c _) ->
      PinResponseInvalidResponseCode c bt
