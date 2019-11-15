module Money where

import PositiveInteger

data Money = Credit PositiveInteger
           | Debit PositiveInteger
  deriving (Eq, Ord, Show)

-- constructor for Money
money :: Integer -> Money
money n 
  | n < 0 = Debit $ positiveInteger (-n)
  | otherwise = Credit $ positiveInteger n

toCents :: Money -> Integer
toCents (Credit cents) = unPositiveInteger cents
toCents (Debit cents) = unPositiveInteger cents

-- Credit <-> Debit
negate :: Money -> Money
negate (Credit amount) = Debit amount
negate (Debit amount) = Credit amount

add :: Money -> Money -> Money
add a b = money $ (toCents a) + (toCents b)

-- returns a tuple (dollars, cents) for presentation
dollars_and_cents :: Money -> (Integer, Integer)
dollars_and_cents money = quotRem (toCents money) 100
