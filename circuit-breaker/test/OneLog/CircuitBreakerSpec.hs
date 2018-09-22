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

  it "sets timestamp and increment state when receiving error given state is initial" $ do
    ref <- newIORef $ CurrentState Nothing 0

    let entry = LogEntry time (Message "petstore" $ encode (Error InvalidPayment))
    controlCircuit ref entry
      `shouldReturn` [ LogEntry time (Message "controller" $ encode (CurrentState (Just time) 1)), entry ]
