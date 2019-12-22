{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module OneLog.CircuitBreaker where

import           Control.Concurrent.Chan
import           Control.Monad.Reader
import           Data.Aeson              (FromJSON, ToJSON, encode)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Lazy    as LBS
import           Data.IORef
import           Data.Time.Clock
import           GHC.Generics
import           Log.Control
import           PetStore.Control
import           PetStore.Messages
import OneLog.CircuitBreakerSM

-- * Low-level Operations on Circuit Breaker

-- ** Types

data CircuitBreaker = CircuitBreaker { controlChannel :: Chan ByteString
                                     , stateRef :: IORef CurrentState
                                     }

-- ** Operations

initialState :: CurrentState
initialState = NotBroken Nothing 0

logCircuitBreaker :: ToJSON a => UTCTime -> a -> LogEntry
logCircuitBreaker ts = LogEntry ts . Message "circuit-breaker" . encode

sendCommand :: CircuitBreakerM m => ToJSON a => a -> m ()
sendCommand c = asks controlChannel >>= liftIO . flip writeChan (LBS.toStrict $ encode c)

changeState :: CircuitBreakerM m => CurrentState -> m ()
changeState st = asks stateRef >>= liftIO . flip writeIORef st

getState :: CircuitBreakerM m => m CurrentState
getState = asks stateRef >>= liftIO . readIORef

type CircuitBreakerM m = (MonadReader CircuitBreaker m, MonadIO m)

-- * Circuit Breaker Logic

controlCircuit :: CircuitBreaker -> Controller
controlCircuit circuitBreaker entry@(LogEntry _ Message{..}) = flip runReaderT circuitBreaker $ do
  (controlMessage, logOutput, newState) <- getState >>= pure . circuitBreakerFSM entry
  changeState newState
  maybe (pure ()) sendCommand controlMessage
  pure logOutput

controlCircuit _c EndOfLog = pure <$> logEntry "circuit-breaker" "\"Exiting\""
