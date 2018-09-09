{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module PetStore.Control where

import           Control.Concurrent.Async
import           Data.Aeson
import           Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.Functor             (void)
import           Data.IORef
import           GHC.Generics
import           PetStore.Config
import           PetStore.Payment.Api
import           System.IO                (stdin)

data Command = BreakCircuit | RestoreCircuit
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

startControl :: ServerConfig -> IORef PaymentClient -> IO (Async ())
startControl ServerConfig{..} paymentClient = readIORef paymentClient >>= async . go
  where
    go pay = do
      bs <- BS.hGetLine stdin
      void $ case decode (LBS.fromStrict bs) of
        Just BreakCircuit -> atomicModifyIORef' paymentClient $ \ _ -> (nullClient, ())
        Just RestoreCircuit -> atomicModifyIORef' paymentClient $ \ _ -> (pay, ())
        Nothing -> pure ()
      go pay
