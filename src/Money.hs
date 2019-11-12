module Money where

data Money = Money { cents :: Integer }

zeroCents = Money(0)

add :: Money -> Money -> Money
add money1 money2 = Money $ cents money1 + cents money2

subtract :: Money -> Money -> Money
subtract money1 money2 = add money1 $ Money (-cents money2)

dollars_and_cents :: Money -> (Integer, Integer)
dollars_and_cents (Money cents) = quotRem cents 100
