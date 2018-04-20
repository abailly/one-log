#!/usr/bin/env stack
-- stack runhaskell  --

{-|
A simple script to build and run all components of the ''One Log'' talk
-}

import           Data.Monoid
import           System.Environment (getArgs)
import           System.Exit
import           System.Process

runProc :: FilePath -> [ String ] -> FilePath -> IO ()
runProc fp arg dir = do
  putStrLn $ "> " <> fp <> " " <> unwords arg
  res <- createProcess ((proc fp arg) { cwd = Just dir }) >>= \ ( _,_,_, h) -> waitForProcess h
  case res of
    ExitSuccess       -> pure ()
    e@(ExitFailure _) -> exitWith e

doBuild :: IO ()
doBuild = do
  runProc "mvn" [ "install" ] "pet-store-payment"
  runProc "stack" [ "test", "--docker" , "--fast" ]  "."
  runProc "stack" [ "install", "--docker" , "--fast" ]  "."
  runProc "stack" [ "image", "container", "--docker" ] "."

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["run"]   -> undefined
    ["build"] -> doBuild
    _         -> putStrLn "expecting one of 'run' or 'build'"
