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
import           Control.Lens
import           Control.Monad        (forM_)
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.String
import           Data.Text            (Text, pack)
import qualified Data.Text.Encoding   as Text
import qualified Data.Text.IO         as Text
import           Data.Vector.Lens     (sliced)
import           Pipes
import qualified Pipes.ByteString     as P
import           Prelude              hiding (id, (.))
import           System.IO
import           Text.Parsec          as Parsec

-- http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Type.html#t:Lens
type Morphism' = forall f . (Applicative f, Contravariant f) => (Value -> f Value) -> Value -> f Value

data Morphism where
  Id :: Morphism
  Tag :: Text -> Morphism
  Idx :: Int -> Morphism
  Rge :: Int -> Int -> Morphism
  Comp :: Morphism -> Morphism -> Morphism

deriving instance Show Morphism
deriving instance Eq Morphism

instance IsString Morphism where
  fromString = Tag . pack

parseMorphism :: String -> Morphism
parseMorphism s =
  case runParser parser () "" s of
    Right m -> m
    Left e  -> error (show e)
  where
    parser = foldl1 Comp <$> (parseTag <|> Parsec.try parseIndex <|> parseRange) `sepBy1` parseDot

    parseTag :: Parsec String () Morphism
    parseTag = Tag . pack <$> many1 alphaNum

    parseIndex :: Parsec String () Morphism
    parseIndex = Idx <$> between (char '[') (char ']') positive

    parseRange :: Parsec String () Morphism
    parseRange = between (char '[') (char ']') $ Rge <$> positive <*> (string ".."  *> positive)

    positive :: Parsec String () Int
    positive = read <$> many1 digit

    parseDot = spaces >> char '.' >> spaces

--fromMorphism :: Morphism -> Traversal' Value Value
fromMorphism :: Morphism -> Morphism'
fromMorphism (Tag s)     = key s
fromMorphism (Idx i)     = nth i
fromMorphism (Rge b e)   = _Array . sliced b e . re _Array
fromMorphism (Comp m m') = fromMorphism m . fromMorphism m'
fromMorphism Id          = id

applyMorphism :: (MonadIO m) => Morphism' -> Pipe Value [Value] m r
applyMorphism morph = do
  v <- await
  yield $ v ^.. morph
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


printValue :: (MonadIO m) => Proxy () [Value] y' y m b
printValue = do
  c <- await
  liftIO $ forM_ c $ Text.putStrLn . Text.decodeUtf8 . LBS.toStrict . encode
  printValue
