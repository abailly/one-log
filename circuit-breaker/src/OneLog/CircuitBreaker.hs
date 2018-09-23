{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module OneLog.CircuitBreaker where

import           Data.Aeson           (FromJSON, ToJSON, encode)
import           Data.ByteString.Lazy (hPutStr)
import           Data.IORef
import           Data.Time.Clock
import           GHC.Generics
import           Log.Control
import           PetStore.Messages
import           System.IO            (hFlush, stderr)

data WrappedLog = WrappedLog { timestamp :: UTCTime, message :: Output }
  deriving (Show, Generic, ToJSON, FromJSON)

data CurrentState = CurrentState { firstError :: Maybe UTCTime
                                 , errorCount :: Int
                                 }
                  deriving(Show, Generic, ToJSON)

initialState :: CurrentState
initialState = CurrentState Nothing 0

controlCircuit :: IORef CurrentState -> Controller
controlCircuit _ref EndOfLog = pure <$> logEntry "circuit-breaker" "\"Exiting\""
controlCircuit ref entry@(LogEntry ts Message{..}) = do
  hPutStr stderr ("Inner message: >>>> " <> msg <> "\n") >> hFlush stderr
  case jsonFromText msg of
    Right (WrappedLog{message = Error InvalidPayment}) -> handlePaymentError ref entry ts
--    Right (CheckedOutBasket{})   -> resetError ref entry
    _                            -> pure [ entry ]

handlePaymentError :: IORef CurrentState -> LogEntry -> UTCTime -> IO [ LogEntry ]
handlePaymentError ref entry ts = do
  st <- readIORef ref
  case st of
    _ -> do
      let newSt = CurrentState (Just ts) 1
      writeIORef ref newSt
      pure [ LogEntry ts (Message "circuit-breaker" $ encode newSt) , entry ]
