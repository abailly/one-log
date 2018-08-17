{-# LANGUAGE ExplicitForAll     #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

{-| Take as input a sequence of JSON values and "morph" them
into a simpler representation.

The goal is to be able to "easily" define transformations on arbitrary
JSON values representing a stream of logs, in order to produce different
"languages".
-}
module OneLog.Morphism where

import           Control.Category
import           Control.Exception    (IOException, try)
import           Control.Lens
import           Control.Monad        (forM_, forever, unless)
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.String
import           Data.Text            (Text, pack)
import qualified Data.Text.IO         as Text
import           Pipes
import qualified Pipes.ByteString     as P
import           Prelude              hiding (id, (.))
import           System.IO
import           Text.Parsec

-- http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Type.html#t:Lens
-- type Lens'' f s t a b = (a -> f b) -> s -> f t

data Morphism where
  Id :: Morphism
  Tag :: Text -> Morphism

deriving instance Show Morphism
deriving instance Eq Morphism

instance IsString Morphism where
  fromString = Tag . pack

-- mkLens :: (Applicative f) => Morphism Value Value -> Lens'' f Value Value Value Value
-- mkLens Id           = id
-- mkLens (Tag s)      = ix s
-- mkLens (Comp  m m') = mkLens m . mkLens m'

parseMorphism :: String -> Morphism
parseMorphism s =
  case runParser parseTag () "" s of
    Right m -> m
    Left e  -> error (show e)
  where
    parseTag :: Parsec String () Morphism
    parseTag = Tag . pack <$> many1 alphaNum

fromMorphism :: Morphism -> Traversal' Value Value
fromMorphism (Tag s) = key s
fromMorphism Id      = id

applyMorphism :: (MonadIO m, Show a, Monoid a) => Traversal' Value a -> Pipe Value a m r
applyMorphism morph = do
  v <- await
  yield $ v ^. morph
  applyMorphism morph

decodeText :: (MonadIO m) => Handle -> Producer Value m ()
decodeText hdl = pump >-> chunker mempty >-> decoder
  where
    pump = P.hGetSome 1024 hdl

    chunker acc = do
      bs <- await
      rest <- yieldChunks (acc <> bs)
      chunker rest

    yieldChunks bs = do
      case BS.break (== 0xa) bs of
        (hd,"") -> pure hd
        (hd,tl) -> yield hd >> yieldChunks (BS.drop 1 tl)

    decoder = do
      bs <- await
      case eitherDecode (LBS.fromStrict bs) of
        Right v -> yield v >> decoder
        Left e  -> liftIO (print e) >> decoder


printText :: (MonadIO m) => Proxy () Text y' y m b
printText = do
  c <- await
  unless (c == "") $ liftIO $ Text.putStrLn c
  printText
