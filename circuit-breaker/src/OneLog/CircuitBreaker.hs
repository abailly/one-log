{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module OneLog.CircuitBreaker where

import           Control.Concurrent.Chan
import           Control.Monad           (when)
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

data CurrentState = CurrentState { firstError :: Maybe UTCTime
                                 , errorCount :: Int
                                 }
                  deriving(Show, Generic, ToJSON)

initialState :: CurrentState
initialState = CurrentState Nothing 0

controlCircuit :: Chan ByteString -> IORef CurrentState -> Controller
controlCircuit _chan _ref EndOfLog = pure <$> logEntry "circuit-breaker" "\"Exiting\""
controlCircuit chan ref entry@(LogEntry ts Message{..}) = do
  case jsonFromText msg of
    Right (WrappedLog{message = Error InvalidPayment}) -> handlePaymentError chan ref entry ts
--    Right (CheckedOutBasket{})   -> resetError ref entry
    _                            -> pure [ entry ]

handlePaymentError :: Chan ByteString -> IORef CurrentState -> LogEntry -> UTCTime -> IO [ LogEntry ]
handlePaymentError chan ref entry ts = do
  st <- readIORef ref
  case st of
    CurrentState Nothing 0 -> resetLastError
    CurrentState (Just startTime) n
      | diffUTCTime ts startTime < 10 -> incrementErrorCount startTime n
      | otherwise                     -> resetLastError
    _ -> pure [ entry ]

  where
    resetLastError = do
      let newSt = CurrentState (Just ts) 1
      writeIORef ref newSt
      pure [ LogEntry ts (Message "circuit-breaker" $ encode newSt) , entry ]

    incrementErrorCount startTime n =  do
      let newSt = CurrentState (Just startTime) (n + 1)
      writeIORef ref newSt
      when (n >= 2) $ writeChan chan (LBS.toStrict $ encode BreakCircuit)
      pure [ LogEntry ts (Message "circuit-breaker" $ encode newSt) , entry ]
