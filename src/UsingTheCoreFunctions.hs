{-# LANGUAGE OverloadedStrings #-}

module UsingTheCoreFunctions where

import qualified Data.Text as T
import Text.Show.Functions

newtype CustomerId = CustomerId Integer deriving (Show)
newtype EmailAddress = EmailAddress String deriving (Show)

data CustomerInfo = CustomerInfo { cid :: CustomerId
                                 , email :: EmailAddress
                                 } deriving (Show)

data Result a = Success a
              | Failure [String]

-- Define Show for Result
instance (Show a) => Show (Result a) where
  show v = case v of
    Success s -> show s
    Failure e -> show e

-- Define Monad Instance
instance Monad Result where
  return a = Success a

  (>>=) xResult f = case xResult of
    Success v -> f v
    Failure errors -> Failure errors

-- Define Applicative Instance
instance Applicative Result where
  pure a = Success a

  (<*>) (Success f) (Success v)  = Success $ f v
  (<*>) (Failure f) (Success v) = Failure f
  (<*>) (Success f) (Failure e) = Failure e
  (<*>) (Failure e1) (Failure e2) = Failure $ e1 ++ e2

-- Define Functor Instance
instance Functor Result where
  fmap f val = case val of
    Success v -> Success $ f v
    Failure e -> Failure e

createCustomerId :: Integer -> Result CustomerId
createCustomerId id
  | id > 0 = Success $ CustomerId id
  | otherwise = Failure ["CustomerId must be positive"]

createEmailAddress :: [Char] -> Result EmailAddress
createEmailAddress str
  | length str == 0 = Failure ["Email must not be empty"]
  | T.isInfixOf "@" $ T.pack str = Success $ EmailAddress str
  | otherwise = Failure ["Email must contain @-sign"]

createCustomer :: CustomerId -> EmailAddress -> CustomerInfo
createCustomer cid email = CustomerInfo { cid = cid, email = email }

-- Applicative Style
createCustomerResultA :: Integer -> String -> Result CustomerInfo
createCustomerResultA cid email =
  let idResult = createCustomerId cid
      emailResult = createEmailAddress email
  in
    createCustomer <$> idResult <*> emailResult

createCustomerResultM :: Integer -> String -> Result CustomerInfo
createCustomerResultM cid email = do
  idResult <- createCustomerId cid
  emailResult <- createEmailAddress email
  Success $ createCustomer idResult emailResult

goodCustomerA = createCustomerResultA 1 "text@example.com"

-- ["CustomerId must be positive"]
badCustomerA = createCustomerResultA 0 "text@example.com"

-- ["Email must contain @-sign"]
badCustomerB = createCustomerResultA 1 "example.com"

-- ["CustomerId must be positive","Email must contain @-sign"]
badCustomerC = createCustomerResultA 0 "example.com"

goodCustomerM1 = createCustomerResultM 1 "test@example.com"
