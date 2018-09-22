{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Log.Control
  ( controlMain, runControl
  , safeDecodeUtf8, jsonFromText, jsonToText, logEntry
  , Controller, LogEntry(..), Message(..)
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Exception        (IOException, catch, finally, throw,
                                           throwIO, try)
import           Control.Monad            (forM_, forever, void, when)
import           Data.Aeson
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LB
import qualified Data.Text.Lazy           as Text
import           Log.Tail
import           System.Environment
import           System.IO                (BufferMode (..), Handle,
                                           hSetBuffering, stdin, stdout)
import           System.IO.Error
import           System.Process

data Control = Control { spawnedProcess :: Async ()
                       , inStream       :: Handle
                       , outStream      :: Async ()
                       , errStream      :: Async ()
                       }

controlMain :: Controller -> IO ()
controlMain controller = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  (process:arguments) <- getArgs
  runControl process arguments "." controller

runControl :: FilePath -> [ String ] -> FilePath
           -> Controller -> IO ()
runControl executable arguments workingDir controller = do
  q <- newChan
  tailer <- tailLogs q controller
  Control{..} <- spawnProc q executable arguments workingDir
  -- process and stream handler threads will terminate "naturally"  when
  -- the process ends or the pipes get closed, respectively
  -- we only wait for the @tail@ thread to finish which shuld be caused when
  -- thread is `BlockedIndefinitelyOnSTM` because all the writer threads have
  -- stopped
  cmd <- readCommands inStream
  void $ waitAnyCancel [ tailer, cmd ]
  where
    readCommands hdl = async $ forever $ do
      bs <- BS.hGetLine stdin
      BS.hPutStr hdl (bs <> "\n")

spawnProc :: LogQueue -> FilePath -> [ String ] -> FilePath -> IO Control
spawnProc queue fp arg dir = do
  let name = Text.pack fp

      readOutput :: Handle -> IO (Async ())
      readOutput hd = async $
        forever ((do
                    l <- LB.fromStrict <$> BS.hGetLine hd
                    writeChan queue =<< logEntry name l)
                 `catch` (\ e  -> do
                             writeChan queue =<< (logEntry name $
                                                  encode $
                                                  object [ "process" .=  name
                                                         , "error" .= show e ])
                             when (isEOFError e) $ throw e
                         ))

      logStart = writeChan queue =<< logEntry "control" (encode $ object [ "message" .= Text.pack ("starting " <> fp <> " " <> unwords arg) ])
      logStop = writeChan queue =<< logEntry "control" (encode $ object [ "message" .= Text.pack ("stopped " <> fp <> " " <> unwords arg) ])

  procStart <- try (createProcess ((proc fp arg) { cwd = Just dir, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }))

  case procStart of
    Right (Just hin, Just hout, Just herr, aProc) -> do
      forM_ [ hin, hout, herr] $ flip hSetBuffering LineBuffering
      outtid <- readOutput hout
      errtid <- readOutput herr
      logStart

      p <- async $ void (waitForProcess aProc) `finally` (logStop >> terminateProcess aProc)
      return $ Control p hin outtid errtid
    Left (e :: IOException) -> throwIO $ userError $ "exception launching process " <> fp <> ": " <> show e
    _      -> throwIO $ userError $ "unexpected result from launching process " <> fp
