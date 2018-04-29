{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Net.OsQuery.Server where

import           Control.Monad.Trans         (liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.Maybe
import           Data.Monoid
import           Data.Text                   (Text)
import           GHC.Generics
import           Net.OsQuery.Api
import           Network.Wai.Handler.Warp    (defaultSettings, setPort)
import           Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import           Servant

data ServerConfig = ServerConfig { certificateFile :: FilePath
                                 , keyFile         :: FilePath
                                 , serverSecret    :: Text
                                 , serverPort      :: Int
                                 , queryConfig     :: Maybe Value
                                 }
                  deriving(Eq, Show, Generic, ToJSON, FromJSON)

eXAMPLE_CONFIG :: Value
eXAMPLE_CONFIG =
  object [ "schedule" .=
           object [ "tls_proc" .=
                    object [ "query" .= ("select pid, name, uid, resident_size from processes order by resident_size desc limit 10;" :: String)
                           , "interval" .= (3 :: Int)
                           ]
                  ]
         , "node_invalid" .= False
         ]

startServer :: ServerConfig -> IO ()
startServer ServerConfig{..} = runTLS tls settings $ server
    where
      tls = tlsSettings certificateFile keyFile
      settings = setPort serverPort defaultSettings
      server = serve osQueryApi handlers
      handlers = enroll :<|> config :<|> doLog

      enroll :: Enroll -> Handler Enrolled
      enroll (Enroll secret)
        | secret == serverSecret = pure $ Enrolled "enrolled"
        | otherwise              = liftIO (putStrLn ("enroll: "  <> show secret)) >> pure FailedEnrolled

      config :: Handler Value
      config = pure $ fromMaybe eXAMPLE_CONFIG queryConfig

      doLog :: Value -> Handler NoContent
      doLog v = liftIO (LBS.putStrLn (encode v)) >> pure NoContent
