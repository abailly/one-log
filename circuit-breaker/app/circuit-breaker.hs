{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
module Main where

import           Data.IORef
import           Log.Control
import           OneLog.CircuitBreaker

main :: IO ()
main = do
  ref <- newIORef initialState
  controlMain (\ chan -> controlCircuit chan ref)
