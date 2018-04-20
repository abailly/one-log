{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module PetStore.Payment.Api where

import           PetStore.Payment.Types
import           Servant
import           Servant.Client

type PaymentApi = "payment"  :> ReqBody '[JSON] Payment :> Post '[JSON] PaymentResult

paymentApi :: Proxy PaymentApi
paymentApi = Proxy

-- * Payment Client

checkPayment :: Payment -> ClientM PaymentResult
checkPayment = client paymentApi
