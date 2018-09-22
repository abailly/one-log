{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module OneLog.CircuitBreaker where

import           Data.Aeson        (ToJSON, encode)
import           Data.IORef
import           Data.Time.Clock
import           GHC.Generics
import           Log.Control
import           PetStore.Messages

data CurrentState = CurrentState { firstError :: Maybe UTCTime
                                 , errorCount :: Int
                                 }
                  deriving(Show, Generic, ToJSON)

controlCircuit :: IORef CurrentState -> Controller
controlCircuit _ref EndOfLog = pure <$> logEntry "controller" "\"Exiting\""
controlCircuit ref entry@(LogEntry ts Message{..}) =
  case jsonFromText msg of
    Right (Error InvalidPayment) -> handlePaymentError ref entry ts
--    Right (CheckedOutBasket{})   -> resetError ref entry
    _                            -> pure [ entry ]

handlePaymentError :: IORef CurrentState -> LogEntry -> UTCTime -> IO [ LogEntry ]
handlePaymentError ref entry ts = do
  st <- readIORef ref
  case st of
    CurrentState Nothing 0 -> do
      let newSt = CurrentState (Just ts) 1
      writeIORef ref newSt
      pure [ LogEntry ts (Message "controller" $ encode newSt) , entry ]
    _ -> pure [ entry ]
