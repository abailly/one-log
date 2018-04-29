{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Net.OsQuery.Api where

import           Data.Aeson
import           Data.Text  (Text)
import           Servant

data Enrolled = Enrolled Text
              | FailedEnrolled

instance ToJSON Enrolled where
  toJSON (Enrolled k)   = object [ "node_key" .= k ]
  toJSON FailedEnrolled = object [ "node_invalid" .= True ]

newtype Enroll = Enroll { enroll_secret :: Text }

instance FromJSON Enroll where
  parseJSON = withObject "Enroll" $ \ o -> Enroll <$> o .: "enroll_secret"

type OsQueryApi = "enroll" :> ReqBody '[JSON] Enroll :> Post '[JSON] Enrolled
                  :<|> "config" :> Post '[JSON] Value
                  :<|> "logger" :> ReqBody '[JSON] Value :> Post '[JSON]  NoContent


osQueryApi :: Proxy OsQueryApi
osQueryApi = Proxy
