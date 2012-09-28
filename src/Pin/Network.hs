{-# LANGUAGE FlexibleContexts #-}
module Pin.Network where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import qualified Data.ByteString.Char8 as B8
import Data.Certificate.X509 (X509)

import Network.HTTP.Conduit
import Network.TLS (TLSCertificateUsage)
import Network.TLS.Extra (certificateVerifyDomain)


checkDomainOnly :: B8.ByteString -> [X509] -> IO TLSCertificateUsage
checkDomainOnly host' certs = return $ certificateVerifyDomain (B8.unpack host') certs

withCustomManager :: (MonadIO m, MonadBaseControl IO m, MonadThrow m, MonadUnsafeIO m) =>
  ManagerSettings -> (Manager -> ResourceT m a) -> m a
withCustomManager settings f = runResourceT $
    allocate (newManager settings) closeManager >>= \(_, manager) -> f manager


