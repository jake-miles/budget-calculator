module Money where

import PositiveInteger

data PositiveMoney = PositiveMoney PositiveInteger
  deriving (Eq, Ord, Show)

data Money = Money { unMoney :: Integer }
  deriving (Eq, Ord, Show)

positiveMoney :: Integer -> PositiveMoney
positiveMoney n = PositiveMoney $ positiveInteger n

toMoney :: PositiveMoney -> Money
toMoney (PositiveMoney cents) = Money $ unPositiveInteger cents

plus :: Money -> Money -> Money
plus (Money a) (Money b) = Money $ a + b

minus :: Money -> Money -> Money
minus a b = a `plus` Money.negate b

negate :: Money -> Money
negate (Money cents) = Money $ -cents

dollars_and_cents :: Money -> (Integer, Integer)
dollars_and_cents m = quotRem (unMoney m) 100
