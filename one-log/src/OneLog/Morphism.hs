{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE RankNTypes     #-}

{-| Take as input a sequence of JSON values and "morph" them
into a simpler representation.

The goal is to be able to "easily" define transformations on arbitrary
JSON values representing a stream of logs, in order to produce different
"languages".
-}
module OneLog.Morphism where

import           Control.Category
import           Control.Lens
import           Control.Monad        (forM_)
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Proxy
import           Data.String
import           Data.Text            (Text, pack)
import           Pipes                hiding (Proxy)
import qualified Pipes.ByteString     as P
import           Prelude              hiding (id, (.))
import           System.IO

-- http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Type.html#t:Lens
-- type Lens'' f s t a b = (a -> f b) -> s -> f t

data Morphism a b where
  Id :: Morphism a a
  Tag :: Text -> Morphism a b
  Comp :: Morphism b c -> Morphism a b -> Morphism a c

instance IsString (Morphism a b) where
  fromString = Tag . pack

instance Category Morphism where
  id = Id
  (.) = Comp

-- mkLens :: (Applicative f) => Morphism Value Value -> Lens'' f Value Value Value Value
-- mkLens Id           = id
-- mkLens (Tag s)      = ix s
-- mkLens (Comp  m m') = mkLens m . mkLens m'

applyMorphism :: (MonadIO m, Show a, Monoid a) => Traversal' Value a -> Pipe Value a m r
applyMorphism morph = do
  v <- await
  yield $ v ^. morph
  applyMorphism morph

decodeText :: (MonadIO m) => Handle -> Producer Value m ()
decodeText hdl = P.hGetSome 1024 hdl >-> chunker mempty >-> decoder
  where
    chunker acc = do
      bs <- await
      case BS.break (== fromIntegral (fromEnum '\n')) bs of
        (hd,"") -> chunker (acc <> hd)
        (hd,tl) -> yield (acc <> hd) >> chunker (BS.drop 1 tl)

    decoder = do
      bs <- await
      case eitherDecode (LBS.fromStrict bs) of
        Right v -> yield v >> decoder
        Left _  -> decoder
