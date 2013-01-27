{-# LANGUAGE OverloadedStrings #-}
module Network.Api.Pin.Core (charge) where

import qualified Data.ByteString.Lazy as BL
import Data.Attoparsec.Lazy
import Data.Aeson
import Data.Text

import Network.Api.Support
import Network.Api.Pin.Data
import Network.HTTP.Conduit
import Network.HTTP.Types


charge :: PinConfig -> PinRequest -> IO PinResponse
charge conf req =
  runRequest (pinManagerSettings conf) (pinUrl conf) (
      (setApiKey . pinApiKeyBS $ conf) <&>
      (setParams . toParams $ req) <&>
      setPost <&>
      setHeaders [("Accept", "application/json")]
    ) (basicResponder responder)

charge :: PinConfig -> PinRequest -> IO PinResponse
charge conf req =
  runRequest (pinManagerSettings conf) (pinUrl conf) (
      (setApiKey . pinApiKeyBS $ conf) <&>
      (setParams . toParams $ req) <&>
      setPost <&>
      setHeaders [("Accept", "application/json")]
    ) (basicResponder responder)

responder :: Int -> BL.ByteString -> PinResponse
responder 200 body = parseBodyWith body (syntaxErr 200 body) (formatErr 200 body) successToPinResponse
responder 401 _    = PinResponseUnauthorized
responder 422 body = parseBodyWith body (syntaxErr 422 body) (formatErr 422 body) unprocessibleToPinResponse
responder 500 body = PinResponseServerError (toText body)
responder c   body = PinResponseInvalidResponseCode c (toText body)
