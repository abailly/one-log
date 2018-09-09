{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Control.Concurrent.Async
import           Data.Functor
import           Data.IORef
import           PetStore.Config
import           PetStore.Control
import           PetStore.Payment.Api
import           PetStore.Server
import           System.Environment

main :: IO ()
main = do
  [devMode, port, paymentHost, paymentPort ] <- getArgs
  let config = ServerConfig (read devMode) (read port) paymentHost (read paymentPort)
  paymentClient <- makeClient paymentHost (read paymentPort) >>= newIORef
  server <- startServer config paymentClient
  control <- startControl config paymentClient
  void $ waitAnyCancel [ server, control ]
