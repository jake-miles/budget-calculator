module PositiveMoney where

import PositiveInteger
import Money

data PositiveMoney = PositiveMoney PositiveInteger
  deriving (Eq, Ord, Show)

positiveMoney :: Integer -> PositiveMoney
positiveMoney = PositiveMoney . positiveInteger 

toMoney :: PositiveMoney -> Money
toMoney (PositiveMoney cents) = Money $ unPositiveInteger cents

