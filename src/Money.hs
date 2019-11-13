module Money where

data Money = Money { cents :: Integer }
  deriving (Eq, Show)

add :: Money -> Money -> Money
add money1 money2 = Money $ cents money1 + cents money2

subtract :: Money -> Money -> Money
subtract money1 money2 = add money1 $ debit money2

debit :: Money -> Money
debit (Money _cents) = Money (-_cents)

dollars_and_cents :: Money -> (Integer, Integer)
dollars_and_cents (Money _cents) = quotRem _cents 100
