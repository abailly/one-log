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
      ref <- newIORef $ NotBroken Nothing 0
      chan <- newControlChannel

      let entry = LogEntry time (Message "petstore" $ encode (WrappedLog time (Error InvalidPayment)))
      controlCircuit (CircuitBreaker chan ref) entry
        `shouldReturn` [ LogEntry time (Message "circuit-breaker" $ encode (NotBroken (Just time) 1)), entry ]

    it "increments state given state is not initial and timestamp is within 10s" $ do
      let fiveSecondsLater = addUTCTime 5 time
      ref <- newIORef $ NotBroken (Just time) 1
      chan <- newControlChannel

      let entry = LogEntry fiveSecondsLater (Message "petstore" $ encode (WrappedLog time (Error InvalidPayment)))
      controlCircuit (CircuitBreaker chan ref) entry
        `shouldReturn` [ LogEntry fiveSecondsLater (Message "circuit-breaker" $ encode (NotBroken (Just time) 2)), entry ]

    it "resets timestamp given state is not initial and timestamp is over 10s" $ do
      let tenSecondsLater = addUTCTime 10 time
      ref <- newIORef $ NotBroken (Just time) 1
      chan <- newControlChannel

      let entry = LogEntry tenSecondsLater (Message "petstore"  $ encode (WrappedLog time (Error InvalidPayment)))
      controlCircuit (CircuitBreaker chan ref) entry
        `shouldReturn` [ LogEntry tenSecondsLater (Message "circuit-breaker" $ encode (NotBroken (Just tenSecondsLater) 1)), entry ]

    it "breaks circuit when error count reaches 3" $ do
      let oneSecondsLater = addUTCTime 1 time
      ref <- newIORef $ NotBroken (Just time) 2
      chan <- newControlChannel

      let entry = LogEntry oneSecondsLater (Message "petstore"  $ encode (WrappedLog time (Error InvalidPayment)))
      controlCircuit (CircuitBreaker chan ref) entry
        `shouldReturn` [ LogEntry oneSecondsLater (Message "circuit-breaker" $ encode (Broken oneSecondsLater)), entry ]

      decode . LBS.fromStrict <$> readChan chan `shouldReturn` Just BreakCircuit

  describe "when circuit is broken" $ do

    it "half-open circuit given 30s have passed letting through one request" $ do
      let thirtySecondsLater = addUTCTime 30 time
      ref <- newIORef $ Broken time
      chan <- newControlChannel

      let entry = LogEntry thirtySecondsLater (Message "petstore"  $ encode (WrappedLog time (UserLoggedIn $ User "bob")))

      controlCircuit (CircuitBreaker chan ref) entry
        `shouldReturn` [ LogEntry thirtySecondsLater (Message "circuit-breaker" $ encode (HalfBroken time)), entry ]

      decode . LBS.fromStrict <$> readChan chan `shouldReturn` Just RestoreCircuit

  describe "when circuit is half-open" $ do

    it "restores circuit given payment succeeds" $ do
      ref <- newIORef $ HalfBroken time
      chan <- newControlChannel

      let entry = LogEntry time (Message "petstore"  $ encode (WrappedLog time (CheckedOutBasket (User "bob") (Payment "1234") 100)))

      controlCircuit (CircuitBreaker chan ref) entry
        `shouldReturn` [ LogEntry time (Message "circuit-breaker" $ encode (NotBroken Nothing 0)), entry ]

    it "close circuit again given payment fails" $ do
      let tenSecondsLater = addUTCTime 10 time
      ref <- newIORef $ HalfBroken time
      chan <- newControlChannel

      let entry = LogEntry tenSecondsLater (Message "petstore"  $ encode (WrappedLog tenSecondsLater (Error InvalidPayment)))

      controlCircuit (CircuitBreaker chan ref) entry
        `shouldReturn` [ LogEntry tenSecondsLater (Message "circuit-breaker" $ encode (Broken tenSecondsLater)), entry ]

      decode . LBS.fromStrict <$> readChan chan `shouldReturn` Just BreakCircuit
