#!/usr/bin/env stack
-- stack runhaskell --package async --package unix --

{-|
A simple script to build and run all components of the ''One Log'' talk
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror #-}

import           Color
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Exception        (IOException, catch, finally, throw,
                                           throwIO, try)
import           Control.Monad            (forever, void)
import           Data.Aeson
import qualified Data.ByteString.Char8    as BS8
import qualified Data.ByteString.Lazy     as LBS
import           Data.Monoid
import qualified Data.Text.Lazy           as Text
import           Log
import           System.Environment       (getArgs)
import           System.Exit
import           System.IO
import           System.IO.Error          (isEOFError)
import           System.Process

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["run"]   -> doRun
    ["build"] -> doBuild
    _         -> putStrLn "expecting one of 'run' or 'build'"


doBuild :: IO ()
doBuild = do
  runProc "mvn" [ "install" ] "pet-store-payment"
  runProc "stack" [ "test",  "--fast" ]  "."
  runProc "stack" [ "install",  "--fast" ]  "."


doRun :: IO ()
doRun = do
  q  <- newChan
  t  <- tailLogs q stdout
  let [ c1, c2, c3, c4 ] = take 4 $ genColors
  j  <- spawnProc c1 q "java" [ "-jar" , "./pet-store-payment/target/pet-store-payment-1.0-SNAPSHOT.jar", "server", "payment-conf.yaml" ] "."
  h  <- spawnProc c2 q "pet-store-server" [ "Dev" , "9090", "localhost", "8080" ] "."
  os <- spawnProc c3 q "osquerys" [ "osquery.conf", "test_server.pem" , "test_server.key", "8088" ] "."
  oq <- spawnProc c4 q "osqueryd" [ "--verbose",  "--ephemeral", "--disable_database", "--tls_hostname",  "localhost:8088"
                               , "--tls_server_certs",  "test_server_ca.pem"
                               , "--config_plugin", "tls"
                               , "--config_tls_endpoint",  "/config"
                               , "--logger_tls_endpoint", "/logger"
                               , "--logger_plugin",  "tls"
                               , "--enroll_tls_endpoint", "/enroll", "--enroll_secret_path", "secret.txt"
                               ] "."

  void $ waitAnyCancel ([ t ] <> j <> h <> os <> oq)


runProc :: FilePath -> [ String ] -> FilePath -> IO ()
runProc fp arg dir = do
  putStrLn $ "> " <> fp <> " " <> unwords arg
  res <- createProcess ((proc fp arg) { cwd = Just dir }) >>= \ ( _,_,_, h) -> waitForProcess h
  case res of
    ExitSuccess       -> pure ()
    e@(ExitFailure _) -> exitWith e


spawnProc :: Color -> LogQueue -> FilePath -> [ String ] -> FilePath -> IO [ Async () ]
spawnProc col queue fp arg dir = do
  let name = Text.pack fp

      readOutput :: Handle -> IO (Async ())
      readOutput hd = async $
        forever ((do
                    l <- LBS.fromStrict <$> BS8.hGetLine hd
                    writeChan queue (logEntry name col l))
                 `catch` (\ e  ->
                             if isEOFError e
                             then throw e
                             else writeChan queue (logEntry name col $
                                                   encode $
                                                   object [ "process" .=  name
                                                          , "error" .= show e ])))

  procStart <- try (createProcess ((proc fp arg) { cwd = Just dir, std_in = Inherit, std_out = CreatePipe, std_err = CreatePipe }))

  case procStart of
    Right (_, Just aout, Just aerr, aProc) -> do
      outtid <- readOutput aout
      errtid <- readOutput aerr
      writeChan queue $ logEntry "driver" (Color 200 10 10) (encode $ object [ "message" .= Text.pack ("starting " <> fp <> " " <> unwords arg) ])

      p <- async $ void (waitForProcess aProc) `finally` terminateProcess aProc
      return $ [p, outtid, errtid]
    Left (e :: IOException) -> throwIO $ userError $ "exception launching process " <> fp <> ": " <> show e
    _      -> throwIO $ userError $ "unexpected result from launching process " <> fp
