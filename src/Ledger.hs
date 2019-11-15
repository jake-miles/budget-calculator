module Ledger where

import Data.Time (Day)
import Data.List (sortOn)
import Money

type What = String

data Account = Account String
  deriving (Eq, Show)

data ExternalEntity = ExternalEntity String
  deriving (Eq, Show)

data Balance = Balance {
  balanceDate :: Day,
  balanceAmount :: Money,
  balanceAccount :: Account
}
  deriving (Eq, Show)

data Event = Event {
  eventDate :: Day,
  eventAmount :: PositiveMoney, 
  eventTransaction :: Transaction
}
  deriving (Eq, Show)

data Transaction = Income Account (Maybe ExternalEntity) (Maybe What)
                 | Expense Account (Maybe ExternalEntity) (Maybe What)
                 | Transfer FromAccount ToAccount
                 | StartOfDayBalance Account

type FromAccount = Account
type ToAccount = Account

find :: (a -> Bool) -> [a] -> Maybe[a]
find match xs = case (filter match xs) of
  [] -> Nothing
  [x:_] -> Just x

orElse :: a -> Maybe a -> a
orElse defaultValue Nothing = defaultValue
orElse _ (Just x) = x

balanceHistory :: [Event] -> [Balance]
balanceHistory events =
  foldl' applyEvent [] $ sortOn eventDate events

-- todo: should the event be or have a function? (dynamic dispatch)
applyEvent :: [Balance] -> Event -> [Balance]
applyEvent existingBalances event =
  let
    findBalance balances = find (==balanceAccount) balances
    zero = Balance (eventDate event) Money(0)
    getBalance account =
      balanceAmount $ orElse (zero account) $ (findBalance existingBalances account)
    updatedAndNew = enact getBalance event
    untouched = filter (not . findBalance updatesAndNew . balanceAccount) existingBalances    
  in
    untouched ++ updatedAndNew

enact :: (Account -> Balance) -> Event -> [Balance]
enact getBalance (Event date amount transaction) =
  let
    set account newBalance =
      Balance date (newBalance $ getBalance account) account
  in case transaction of
    StartOfDayBalance account -> [set account $ const amount]    
    Income account _ _ -> [set account $ \balance -> Money.add balance amount]
    Expense account _ _ -> [set account $ \balance -> Money.add balance amount]
    Transfer from to ->
      [
        set to $ \balance -> Money.add balance amount,
        set from $ \balance -> Money.add balance $ Money.negate amount
      ]



