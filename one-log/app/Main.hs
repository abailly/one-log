{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import           OneLog.Morphism
import           Options.Generic
import           Pipes
import           System.IO

data Options w = Options { inputFile :: w ::: FilePath <?> "Input file"
                         , expression :: w ::: String <?> "Lens expression to apply to inputs"
                         }
    deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  Options{..} <- unwrapRecord "Lens extractor"
  withFile inputFile ReadMode $ \ hdl ->
    runEffect $ decodeText hdl >-> applyMorphism (fromMorphism $ parseOptics expression) >-> printText
