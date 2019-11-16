module Money (Money (..), 
              zero, plus, minus, Money.negate,
              dollars_and_cents) where

newtype Money = Money { unMoney :: Integer }
  deriving (Eq, Ord, Show)

zero = Money 0

plus :: Money -> Money -> Money
plus (Money a) (Money b) = Money $ a + b

minus :: Money -> Money -> Money
minus a b = a `plus` (Money.negate b)

negate :: Money -> Money
negate (Money cents) = Money (-cents)

dollars_and_cents :: Money -> (Integer, Integer)
dollars_and_cents m = quotRem (unMoney m) 100
