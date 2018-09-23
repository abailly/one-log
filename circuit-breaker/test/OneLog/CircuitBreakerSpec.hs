module OneLog.CircuitBreakerSpec where

import           Control.Concurrent.Chan
import           Data.Aeson              (decode, encode)
import qualified Data.ByteString.Lazy    as LBS
import           Data.IORef
import           Data.Time.Clock
import           Log.Control
import           OneLog.CircuitBreaker
import           PetStore.Control
import           PetStore.Messages
import           Test.Hspec

spec :: Spec
spec = describe "Circuit Breaker" $ do
  let time = UTCTime (toEnum 42) 0

  describe "when receiving error" $ do

    it "sets timestamp and increment state given state is initial" $ do
      ref <- newIORef $ CurrentState Nothing 0
      chan <- newControlChannel

      let entry = LogEntry time (Message "petstore" $ encode (WrappedLog time (Error InvalidPayment)))
      controlCircuit chan ref entry
        `shouldReturn` [ LogEntry time (Message "circuit-breaker" $ encode (CurrentState (Just time) 1)), entry ]

    it "increments state given state is not initial and timestamp is within 10s" $ do
      let fiveSecondsLater = addUTCTime 5 time
      ref <- newIORef $ CurrentState (Just time) 1
      chan <- newControlChannel

      let entry = LogEntry fiveSecondsLater (Message "petstore" $ encode (WrappedLog time (Error InvalidPayment)))
      controlCircuit chan ref entry
        `shouldReturn` [ LogEntry fiveSecondsLater (Message "circuit-breaker" $ encode (CurrentState (Just time) 2)), entry ]

    it "resets timestamp given state is not initial and timestamp is over 10s" $ do
      let tenSecondsLater = addUTCTime 10 time
      ref <- newIORef $ CurrentState (Just time) 1
      chan <- newControlChannel

      let entry = LogEntry tenSecondsLater (Message "petstore"  $ encode (WrappedLog time (Error InvalidPayment)))
      controlCircuit chan ref entry
        `shouldReturn` [ LogEntry tenSecondsLater (Message "circuit-breaker" $ encode (CurrentState (Just tenSecondsLater) 1)), entry ]

    it "breaks circuit when error count reaches 3" $ do
      let oneSecondsLater = addUTCTime 1 time
      ref <- newIORef $ CurrentState (Just time) 2
      chan <- newControlChannel

      let entry = LogEntry oneSecondsLater (Message "petstore"  $ encode (WrappedLog time (Error InvalidPayment)))
      _outs <- controlCircuit chan ref entry

      decode . LBS.fromStrict <$> readChan chan `shouldReturn` Just BreakCircuit
