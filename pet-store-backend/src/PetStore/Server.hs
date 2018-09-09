{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module PetStore.Server where

import           Control.Concurrent.Async
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Default
import           Data.IORef
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

startServer :: ServerConfig -> IORef PaymentClient -> IO (Async ())
startServer conf@ServerConfig{..} clientRef = do
  mlog $ object [ "action" .= ("start" :: String), "configuration" .= conf ]
  store <- makeStore
  logger <- doLog operationMode
  async $ run listeningPort $ logger $ server store operationMode clientRef
    where
      doLog _ = mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }

      runServer store = Handler . flip runReaderT store

      server store Prod paymentClient = serve petStoreApi $ hoistServer petStoreApi (runServer store) (prodHandler paymentClient)
      server store Dev  paymentClient = serve devPetStoreApi $ hoistServer devPetStoreApi (runServer store) (devHandler paymentClient)

      prodHandler paymentClient = listPets :<|> addPet :<|> removePet :<|> login :<|> logout :<|> addToBasket :<|> removeFromBasket :<|> checkout paymentClient :<|> listBasket
      devHandler  paymentClient = prodHandler paymentClient :<|> reset :<|> pure petStoreSwagger
