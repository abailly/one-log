{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module OneLog.CircuitBreaker where

import           Control.Concurrent.Chan
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

data CurrentState = NotBroken { firstError :: Maybe UTCTime
                              , errorCount :: Int
                              }
                  | Broken { breakTime :: UTCTime }
                  deriving(Show, Generic, ToJSON)

initialState :: CurrentState
initialState = NotBroken Nothing 0

controlCircuit :: Chan ByteString -> IORef CurrentState -> Controller
controlCircuit _chan _ref EndOfLog = pure <$> logEntry "circuit-breaker" "\"Exiting\""
controlCircuit chan ref entry@(LogEntry ts Message{..}) = do
  case jsonFromText msg of
    Right (WrappedLog{message = Error InvalidPayment}) -> handlePaymentError chan ref entry ts
    Right (WrappedLog _ _)   -> resetError ref entry
    _                            -> pure [ entry ]

resetError :: Chan ByteString -> IORef CurrentState -> LogEntry -> IO [ LogEntry ]
resetError chan ref entry = do
  st <- readIORef ref
  case st of
    Broken{breakTime} -> case  entry of


handlePaymentError :: Chan ByteString -> IORef CurrentState -> LogEntry -> UTCTime -> IO [ LogEntry ]
handlePaymentError chan ref entry ts = do
  st <- readIORef ref
  case st of
    NotBroken Nothing 0 -> resetLastError
    NotBroken (Just startTime) n
      | diffUTCTime ts startTime < 10 -> incrementErrorCount startTime n
      | otherwise                     -> resetLastError
    _ -> pure [ entry ]

  where
    resetLastError = do
      let newSt = NotBroken (Just ts) 1
      writeIORef ref newSt
      pure [ LogEntry ts (Message "circuit-breaker" $ encode newSt) , entry ]

    incrementErrorCount startTime n =  do
      newState <- if (n >= 2)
        then breakCircuit
        else updateErrorCount startTime (n + 1)
      pure [ LogEntry ts (Message "circuit-breaker" $ encode newState) , entry ]

    breakCircuit = do
      let newSt = Broken ts
      writeChan chan (LBS.toStrict $ encode BreakCircuit)
      return newSt

    updateErrorCount startTime n = do
      let newSt = NotBroken (Just startTime) n
      writeIORef ref newSt
      pure newSt
