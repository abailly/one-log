#!/usr/bin/env stack
-- stack runhaskell  --package async --package unix --

{-|
A simple script to build and run all components of the ''One Log'' talk
-}

import           Control.Concurrent.Async
import           Control.Exception        (finally)
import           Control.Monad            (void)
import           Data.Monoid
import           System.Environment       (getArgs)
import           System.Exit
import           System.Posix.Temp
import           System.Process

runProc :: FilePath -> [ String ] -> FilePath -> IO ()
runProc fp arg dir = do
  putStrLn $ "> " <> fp <> " " <> unwords arg
  res <- createProcess ((proc fp arg) { cwd = Just dir }) >>= \ ( _,_,_, h) -> waitForProcess h
  case res of
    ExitSuccess       -> pure ()
    e@(ExitFailure _) -> exitWith e

spawnProc :: FilePath -> [ String ] -> FilePath -> IO (Async ExitCode)
spawnProc fp arg dir = do
  (fpout, hdlout) <- mkstemps (fp <> "-") ".out"
  (fperr, hdlerr) <- mkstemps (fp <> "-") ".err"
  a <- async $ do
    (_,_,_,hdl) <- createProcess ((proc fp arg) { cwd = Just dir, std_out = UseHandle hdlout, std_err = UseHandle hdlerr })
    waitForProcess hdl `finally` terminateProcess hdl
  putStrLn $ "> " <> fp <> " " <> unwords arg <> "2> " <> fperr <> " 1> " <> fpout
  pure a

doBuild :: IO ()
doBuild = do
  runProc "mvn" [ "install" ] "pet-store-payment"
  runProc "stack" [ "test",  "--fast" ]  "."
  runProc "stack" [ "install",  "--fast" ]  "."


doRun :: IO ()
doRun = do
  j <- spawnProc "java" [ "-jar" , "./pet-store-payment/target/pet-store-payment-1.0-SNAPSHOT.jar", "server", "payment-conf.yaml" ] "."
  h <- spawnProc "stack" [ "exec" ,"pet-store-server", "--", "Prod" , "9090", "localhost", "8080" ] "."
  void $ waitAnyCancel [ j, h]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["run"]   -> doRun
    ["build"] -> doBuild
    _         -> putStrLn "expecting one of 'run' or 'build'"
