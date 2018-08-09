{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
import           Options.Generic
import           PetStore.Driver

data Options w = Options { numThreads :: w ::: Int <?> "Number of threads (e.g. clients) to run driver with"
                         , serverHost :: w ::: String <?> "Hostname/IP of server"
                         , serverPort :: w ::: Int <?> "Port of server"
                         }
    deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  Options{..} <- unwrapRecord "Pet-Store Driver"
  runTestDriver numThreads serverHost serverPort
