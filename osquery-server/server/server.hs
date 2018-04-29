{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

import           Data.Aeson                 (decode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Net.OsQuery.Server
import           System.Environment

main :: IO ()
main = do
  [ confFile, certificateFile, keyFile, read -> serverPort ] <- getArgs
  let serverSecret = "password"
  queryConfig <- decode <$> LBS.readFile confFile
  startServer ServerConfig{..}
