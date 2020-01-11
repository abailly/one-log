{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module OneLog.CircuitBreakerSM where

import           Data.Aeson              (FromJSON, ToJSON, encode)
import           Data.Time.Clock
import           GHC.Generics
import           Log.Control
import           PetStore.Control
import           PetStore.Messages


data CurrentState = NotBroken { firstError :: Maybe UTCTime
                              , errorCount :: Int
                              }
                  | Broken { breakTime :: UTCTime }
                  | HalfBroken { breakTime :: UTCTime }
                  deriving(Show, Generic, ToJSON)

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
    Broken{breakTime}
      | diffUTCTime ts breakTime >= 30 -> halfOpenCircuit breakTime
    HalfBroken{}      -> closeCircuit
    _       -> (Nothing, [ entry ], st)
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

data WrappedLog = WrappedLog { timestamp :: UTCTime, message :: Output }
  deriving (Show, Generic, ToJSON, FromJSON)

logCircuitBreaker :: ToJSON a => UTCTime -> a -> LogEntry
logCircuitBreaker ts = LogEntry ts . Message "circuit-breaker" . encode
