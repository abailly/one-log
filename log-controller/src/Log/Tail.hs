{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Log.Tail where

import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Monad            (forM_)
import           Control.Monad.Trans      (MonadIO, liftIO)
import           Data.Aeson
import           Data.Bifunctor           (bimap)
import qualified Data.ByteString.Lazy     as LBS
import           Data.Text.Encoding.Error
import qualified Data.Text.Lazy           as Text
import           Data.Text.Lazy.Encoding
import           Data.Time.Clock
import           System.IO

-- |Entries that can be fed to a `LogQueue`
data LogEntry = LogEntry UTCTime Message
              -- ^A log message originating from some service
              | EndOfLog
              -- ^A /Poison Pill/  to stop the logging thread
  deriving (Eq, Show)

logEntry :: Text.Text -> LBS.ByteString -> IO LogEntry
logEntry o c = do
  ts <- getCurrentTime
  pure $ LogEntry ts (Message o c)

data Message = Message { origin :: Text.Text
                         -- ^An arbitrary label for messages
                       , msg    :: LBS.ByteString
                         -- ^Payload of message. Should be a valid JSON string but if it is not
                         -- the case, it will be wrapped into proper JSON object
                       }
             deriving (Eq, Show)

type LogQueue  = Chan LogEntry

type Controller = LogEntry -> IO [LogEntry]

safeDecodeUtf8 :: LBS.ByteString -> Text.Text
safeDecodeUtf8 = decodeUtf8With lenientDecode

-- | Pull individual `LogEntry` from a queue and pass them to a `Controller` function
tailLogs :: (MonadIO m)
         => LogQueue
         -> Controller
         -> m (Async ())
tailLogs logSource logSink =  liftIO $ async doLog
  where
    doLog = do
      entry   <- readChan logSource
      entries <- logSink entry
      forM_ entries writeLog
      case entry of
        LogEntry _ _ -> doLog
        EndOfLog     -> pure ()

    writeLog EndOfLog = pure ()
    writeLog (LogEntry ts Message{..}) =
      let
        fullMsg = either
                  (const $ object ["node" .= origin, "timestamp" .= ts, "log" .= object [ "message" .= (safeDecodeUtf8 msg :: Text.Text)]])
                  (\ m ->  object ["node" .= origin, "timestamp" .= ts, "log" .= m ])
                  (jsonFromText msg :: Either Text.Text Value)

        msgString = encode fullMsg

      in
        LBS.hPutStr stdout (msgString <> "\n") >> hFlush stdout

jsonFromText :: (FromJSON a) => LBS.ByteString -> Either Text.Text a
jsonFromText = bimap Text.pack id . eitherDecode

jsonToText :: (ToJSON a) => a -> Text.Text
jsonToText = decodeUtf8 . encode
