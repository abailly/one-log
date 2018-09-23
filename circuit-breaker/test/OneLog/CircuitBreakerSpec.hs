module OneLog.CircuitBreakerSpec where

import           Data.Aeson            (encode)
import           Data.IORef
import           Data.Time.Clock
import           Log.Control
import           OneLog.CircuitBreaker
import           PetStore.Messages
import           Test.Hspec

spec :: Spec
spec = describe "Circuit Breaker" $ do
  let time = UTCTime (toEnum 42) 0

  describe "when receiving error" $ do

    it "sets timestamp and increment state given state is initial" $ do
      ref <- newIORef $ CurrentState Nothing 0

      let entry = LogEntry time (Message "petstore" $ encode (WrappedLog time (Error InvalidPayment)))
      controlCircuit ref entry
        `shouldReturn` [ LogEntry time (Message "circuit-breaker" $ encode (CurrentState (Just time) 1)), entry ]

    it "increments state given state is not initial and timestamp is within 10s" $ do
      let fiveSecondsLater = addUTCTime 5 time
      ref <- newIORef $ CurrentState (Just time) 1

      let entry = LogEntry fiveSecondsLater (Message "petstore" $ encode (WrappedLog time (Error InvalidPayment)))
      controlCircuit ref entry
        `shouldReturn` [ LogEntry fiveSecondsLater (Message "circuit-breaker" $ encode (CurrentState (Just time) 2)), entry ]

    it "resets timestamp given state is not initial and timestamp is over 10s" $ do
      let tenSecondsLater = addUTCTime 10 time
      ref <- newIORef $ CurrentState (Just time) 1

      let entry = LogEntry tenSecondsLater (Message "petstore"  $ encode (WrappedLog time (Error InvalidPayment)))
      controlCircuit ref entry
        `shouldReturn` [ LogEntry tenSecondsLater (Message "circuit-breaker" $ encode (CurrentState (Just tenSecondsLater) 1)), entry ]
