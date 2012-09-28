{-# LANGUAGE OverloadedStrings #-}
module Pin.Core (charge) where

import qualified Data.ByteString.Lazy as BL
import Data.Attoparsec.Lazy
import Data.Aeson
import Data.Text

import Network.Api.Support
import Network.HTTP.Conduit
import Network.HTTP.Types

import Pin.Data

charge :: PinConfig -> PinRequest -> IO PinResponse
charge conf req =
  runRequest (pinManagerSettings conf) (pinUrl conf) (
      (setApiKey . pinApiKeyBS $ conf) <&>
      (setParams . toParams $ req) <&>
      setPost <&>
      setHeaders [("Accept", "application/json")]
    ) responder

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
