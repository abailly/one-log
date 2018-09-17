{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Control.Concurrent.Async
import           Control.Monad
import           Data.IORef
import           PetStore.Config
import           PetStore.Control
import           PetStore.Payment.Api
import           PetStore.Server
import           System.Environment
import           System.IO

main :: IO ()
main = do
  forM_ [ stdin, stdout, stderr ] $ flip hSetBuffering LineBuffering
  [devMode, port, paymentHost, paymentPort ] <- getArgs
  let config = ServerConfig (read devMode) (read port) paymentHost (read paymentPort)
  paymentClient <- makeClient paymentHost (read paymentPort) >>= newIORef
  server <- startServer config paymentClient
  control <- startControl config paymentClient
  void $ waitAnyCancel [ server, control ]
