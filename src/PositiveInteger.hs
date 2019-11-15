module PositiveInteger (PositiveInteger, positiveInteger, unPositiveInteger) where

-- wraps an Integer that is guaranteed to be >= 0. 
-- data constructor is not exported - use `positiveInteger` 
newtype PositiveInteger = PositiveInteger { unPositiveInteger :: Integer }
  deriving (Eq, Ord, Show)

-- constructor for PositiveInteger. errors if n < 0.
positiveInteger :: Integer -> PositiveInteger
positiveInteger n
  | n >= 0 = PositiveInteger n
