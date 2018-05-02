{-# LANGUAGE OverloadedStrings #-}
module Color where

import           Data.Monoid
import           Data.Text.Lazy

data Color = Color { r :: Int, g :: Int, b :: Int }
  deriving (Eq, Show)

toS :: (Show a) => a -> Text
toS = pack . show

colorise :: Color -> Text -> Text
colorise (Color r g b) t = "\x1b[38;2;" <> toS r <> ";" <> toS g <> ";" <> toS b <> "m" <> t <> "\x1b[0m"
