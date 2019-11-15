module Money where

import PositiveInteger

data PositiveMoney = PositiveMoney PositiveInteger
  deriving (Eq, Ord, Show)

data NegativeMoney = NegativeMoney PositiveInteger
  deriving (Eq, Ord, Show)

class Money a where

  toCents :: a -> Integer
  negate :: (Money b) => a -> b
  
  add :: (Money b, Money c) => a -> b -> c
  add a b = money $ (toCents a) + (toCents b)
  
  dollars_and_cents :: a -> (Integer, Integer)
  dollars_and_cents a = quotRem (toCents a) 100

instance Money PositiveMoney where
  toCents (PositiveMoney cents) = unPositiveInteger cents
  negate (PositiveMoney cents) = NegativeMoney cents

instance Money NegativeMoney where
  toCents (NegativeMoney cents) = unPositiveInteger cents
  negate (NegativeMoney cents) = PositiveMoney cents

-- constructor for Money
money :: Money a => Integer -> a
money n 
  | n < 0 = NegativeMoney $ positiveInteger (-n)
  | otherwise = PositiveMoney $ positiveInteger n
