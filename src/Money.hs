module Money where

data Money = Money { cents :: Integer }
  deriving (Eq, Ord, Show)

add :: Money -> Money -> Money
add money1 money2 = Money $ cents money1 + cents money2

negate :: Money -> Money
negate (Money _cents) = Money (-_cents)

dollars_and_cents :: Money -> (Integer, Integer)
dollars_and_cents (Money _cents) = quotRem _cents 100
