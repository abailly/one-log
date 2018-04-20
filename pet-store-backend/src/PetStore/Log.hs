{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module PetStore.Log where

import           Control.Monad.Trans
import           Data.Aeson
import           Data.Monoid            ((<>))
import           Data.Time.Clock.System
import           Data.Time.Format
import           Servant                (NoContent)

class MonadLog m where
  mlog :: String -> m ()


withinLog :: (MonadLog m, Monad m, Show b) => String -> m b -> m b
withinLog start act = do
  mlog start
  res <- act
  mlog $ show res
  pure res

instance ToJSON NoContent where
  toJSON _ = Null

instance (MonadIO m) => MonadLog m where
  mlog a = liftIO $ do
    ts <- systemToUTCTime <$> getSystemTime
    Prelude.putStrLn $ "[" <> formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Q")) ts <> "] " <> show a
