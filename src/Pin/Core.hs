{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Pin.Core (charge) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Attoparsec.Lazy
import Data.Aeson
import Data.Text
import Data.Text.Encoding
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE

import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.TLS (TLSCertificateUsage)
import Network.TLS.Extra (certificateVerifyChain, certificateVerifyDomain)
import Data.Certificate.X509 (X509)
import Pin.Data

--charge :: PinRequest -> IO PinResponse
charge :: Text -> PinRequest -> IO Text
charge key req =
  parseUrl (unpack $ "https://test-api.pin.net.au/1/charges") >>= \url ->
  (liftM responder . withManager' (def { managerCheckCerts = verify } ) . httpLbs) (applyBasicAuth (fromText key) "" $ urlEncodedBody (params req) $ url {
      method = "POST"
    , requestHeaders = [
      ("Accept", "application/json")
    ]
    , checkStatus = const . const $ Nothing
  })

verify :: B8.ByteString -> [X509] -> IO TLSCertificateUsage
verify host' certs = return $ certificateVerifyDomain (B8.unpack host') certs

withManager' :: (MonadIO m, MonadBaseControl IO m, MonadThrow m, MonadUnsafeIO m) => ManagerSettings -> (Manager -> ResourceT m a) -> m a
withManager' settings f = runResourceT $ do
    (_, manager) <- allocate (newManager settings) closeManager
    f manager

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
  , ("card[address_line2]", fromText . pinAddress2 . pinAddress $ r)
  , ("card[address_city]", fromText . pinCity . pinAddress $ r)
  , ("card[address_postcode]", fromText . pinPostcode . pinAddress $ r)
  , ("card[address_state]", fromText . pinState . pinAddress $ r)
  , ("card[address_country]", fromText . pinCountry . pinAddress $ r)
  ]

fromInt :: Int -> B.ByteString
fromInt = fromText . pack . show

fromText :: Text -> B.ByteString
fromText = encodeUtf8

responder :: Response BL.ByteString -> Text
responder (Response status _ _ body) =
  let bt = LT.toStrict . LE.decodeUtf8 $ body
   in bt
