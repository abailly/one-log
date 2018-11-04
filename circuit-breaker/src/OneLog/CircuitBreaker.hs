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
  (controlMessage, logOutput, newState) <- getState >>= pure . circuitBreakerFSM entry
  changeState newState
  maybe (pure ()) sendCommand controlMessage
  pure logOutput

controlCircuit _c EndOfLog = pure <$> logEntry "circuit-breaker" "\"Exiting\""

type Transition = (Maybe Command,[ LogEntry], CurrentState)

circuitBreakerFSM :: LogEntry -> CurrentState -> Transition
circuitBreakerFSM entry@(LogEntry _ Message{..}) st =
  case jsonFromText msg of
    Right (WrappedLog{message = Error InvalidPayment})
      -> handlePaymentError entry st
    Right (WrappedLog _ _)
      -> resetError entry st
    _
      -> (Nothing, [ entry ], st)

circuitBreakerFSM EndOfLog st = (Nothing,[],st)

resetError :: LogEntry -> CurrentState -> Transition
resetError entry@(LogEntry ts _) st = do
  case st of
    Broken{breakTime} -> halfOpenCircuit breakTime
    NotBroken{}       -> (Nothing, [ entry ], st)
    HalfBroken{}      -> closeCircuit
  where
    halfOpenCircuit breakTime =
      let newSt = HalfBroken breakTime
      in (Just RestoreCircuit, [ LogEntry ts (Message "circuit-breaker" $ encode newSt), entry ], newSt)

    closeCircuit =
      let  newSt = NotBroken Nothing 0
      in (Nothing, [ LogEntry ts (Message "circuit-breaker" $ encode newSt), entry ], newSt)

resetError _ _ = error "this should never happen"

handlePaymentError :: LogEntry -> CurrentState -> Transition
handlePaymentError entry@(LogEntry ts _) st = do
  case st of
    NotBroken Nothing 0
      -> resetLastError
    NotBroken (Just startTime) n
      | diffUTCTime ts startTime < 10 -> incrementErrorCount startTime n
      | otherwise                     -> resetLastError
    HalfBroken _
      -> breakCircuit
    _
      -> (Nothing, [ entry ], st)

  where
    resetLastError =
      let newSt = NotBroken (Just ts) 1
      in (Nothing, [ logCircuitBreaker ts newSt , entry ], newSt)

    incrementErrorCount startTime n
      | n >= 2    = breakCircuit
      | otherwise = updateErrorCount startTime (n + 1)

    breakCircuit =
      let newSt = Broken ts
      in (Just BreakCircuit, [ logCircuitBreaker ts newSt , entry ], newSt)

    updateErrorCount startTime n =
      let newSt = NotBroken (Just startTime) n
      in (Nothing, [ logCircuitBreaker ts newSt , entry ], newSt)

handlePaymentError EndOfLog _ = error "should never get there"
