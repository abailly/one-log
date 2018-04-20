{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
module PetStore.Payment.Types where

import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.Char           (digitToInt, isDigit)
import           GHC.Generics

data Payment = Payment { cardNumber :: String }
             deriving (Eq,Show,Generic,ToJSON,FromJSON)

checkCardNumber :: Payment -> Bool
checkCardNumber Payment{cardNumber} =
  computeLuhn (reverse cardNumber) 0
  where
    computeLuhn (_:n:rest) k
      | isDigit n = computeLuhn rest (k + reduce n)
      | otherwise = False
    computeLuhn _          k = k == 0

    reduce c =
      let n = 2 * digitToInt c
      in if n > 10
         then (n - 10) + 1
         else n

data PaymentResult = PaymentResult { _id     :: Integer
                                   , _result :: Bool
                                   }
                   | PaymentError { _reason :: String }
  deriving (Eq,Show)

instance ToJSON PaymentResult where
  toJSON (PaymentResult i r) = object [ "id" .= i , "paymentOk" .= r ]
  toJSON (PaymentError r)    = object [ "error" .= r ]

instance FromJSON PaymentResult where
  parseJSON = withObject "PaymentResult" $
              \ o ->
                (PaymentResult <$> o .: "id" <*> o .: "paymentOk")
                <|> (PaymentError <$> o .: "error")
