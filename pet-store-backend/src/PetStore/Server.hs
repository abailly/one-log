{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module PetStore.Server where

--import           Control.Concurrent.MVar
import           Control.Monad.Except
import           Control.Monad.Reader
--import qualified Data.ByteString.Lazy     as LBS
import           Data.Monoid                               ((<>))
--import           Data.Text
--import           Data.Text.Encoding       (encodeUtf8)
import           Data.Default
import           Network.Wai.Handler.Warp                  (run)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.RequestLogger.JSON
import           PetStore.Api
import           PetStore.Config
import           PetStore.Handler
import           PetStore.Log
import           PetStore.Payment.Api
import           PetStore.Store
import           PetStore.Swagger
import           Servant

startServer :: ServerConfig -> IO ()
startServer conf@ServerConfig{..} = do
  mlog $ "Starting PetStore Server: " <> show conf
  store <- makeStore
  paymentClient <- makeClient paymentHost paymentPort
  void $ run listeningPort $ doLog operationMode $ server store operationMode paymentClient
    where
      doLog Dev  = logStdoutDev
      doLog Prod = logStdout

      runServer store = NT $ Handler . flip runReaderT store

      server store Prod paymentClient = serve petStoreApi $ enter (runServer store) (prodHandler paymentClient)
      server store Dev  paymentClient = serve devPetStoreApi $ enter (runServer store) (devHandler paymentClient)

      prodHandler paymentClient = listPets :<|> addPet :<|> removePet :<|> login :<|> logout :<|> addToBasket :<|> removeFromBasket :<|> checkout paymentClient :<|> listBasket
      devHandler  paymentClient = prodHandler paymentClient :<|> reset :<|> pure petStoreSwagger
