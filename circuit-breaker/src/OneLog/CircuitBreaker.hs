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

data WrappedLog = WrappedLog { timestamp :: UTCTime, message :: Output }
  deriving (Show, Generic, ToJSON, FromJSON)

-- * Low-level Operations on Circuit Breaker

-- ** Types

data CircuitBreaker = CircuitBreaker { controlChannel :: Chan ByteString
                                     , stateRef :: IORef CurrentState
                                     }

data CurrentState = NotBroken { firstError :: Maybe UTCTime
                              , errorCount :: Int
                              }
                  | Broken { breakTime :: UTCTime }
                  | HalfBroken { breakTime :: UTCTime }
                  deriving(Show, Generic, ToJSON)

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
  case jsonFromText msg of
    Right (WrappedLog{message = Error InvalidPayment}) -> handlePaymentError entry
    Right (WrappedLog _ _)   -> resetError entry
    _                        -> pure [ entry ]

controlCircuit _c EndOfLog = pure <$> logEntry "circuit-breaker" "\"Exiting\""

resetError :: CircuitBreakerM m => LogEntry -> m [ LogEntry ]
resetError entry@(LogEntry ts _) = do
  st <- getState
  case st of
    Broken{breakTime} -> halfOpenCircuit breakTime
    NotBroken{}       -> pure [ entry ]
    HalfBroken{}      -> closeCircuit
  where
    halfOpenCircuit breakTime = do
      let
        newSt = HalfBroken breakTime
      changeState newSt
      sendCommand RestoreCircuit
      pure [ LogEntry ts (Message "circuit-breaker" $ encode newSt), entry ]
    closeCircuit = do
      let
        newSt = NotBroken Nothing 0
      changeState newSt
      pure [ LogEntry ts (Message "circuit-breaker" $ encode newSt), entry ]

resetError _ = error "this should never happen"

handlePaymentError :: CircuitBreakerM m => LogEntry -> m [ LogEntry ]
handlePaymentError EndOfLog = error "should never get there"
handlePaymentError entry@(LogEntry ts _) = do
  st <- getState
  case st of
    NotBroken Nothing 0 -> resetLastError
    NotBroken (Just startTime) n
      | diffUTCTime ts startTime < 10 -> incrementErrorCount startTime n
      | otherwise                     -> resetLastError
    HalfBroken _ -> breakCircuit
    _ -> pure [ entry ]

  where
    resetLastError = do
      let newSt = NotBroken (Just ts) 1
      changeState newSt
      pure [ logCircuitBreaker ts newSt , entry ]

    incrementErrorCount startTime n
      | n >= 2    = breakCircuit
      | otherwise = updateErrorCount startTime (n + 1)

    breakCircuit = do
      let newSt = Broken ts
      sendCommand BreakCircuit
      pure [ logCircuitBreaker ts newSt , entry ]

    updateErrorCount startTime n = do
      let newSt = NotBroken (Just startTime) n
      changeState newSt
      pure [ logCircuitBreaker ts newSt , entry ]
