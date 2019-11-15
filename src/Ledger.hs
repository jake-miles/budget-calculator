module Ledger where

import Data.Time (Day)
import Data.List 
import PositiveInteger
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
  deriving (Eq, Show)

type FromAccount = Account
type ToAccount = Account

balanceHistory :: [Event] -> [Balance]
balanceHistory events =
  foldl' applyEvent [] $ sortOn eventDate events

-- todo: should the event be or have a function? (dynamic dispatch)
applyEvent :: [Balance] -> Event -> [Balance]
applyEvent existingBalances event =
  let
    findBalance :: [Balance] -> Account -> Maybe Balance
    findBalance balances account = find ((==account) . balanceAccount) balances
    zero = Balance (eventDate event) (Money 0)
    getBalance account =
      maybe (zero account) id $ findBalance existingBalances account
    updatedAndNew = enact getBalance event
    untouched = null . (findBalance updatedAndNew) . balanceAccount
  in
    (filter untouched existingBalances) ++ updatedAndNew

enact :: (Account -> Balance) -> Event -> [Balance]
enact getBalance (Event date positiveAmount transaction) =
  let
    amount = toMoney positiveAmount
    set account calcNewBalance =
      Balance date (calcNewBalance $ balanceAmount $ getBalance account) account
  in case transaction of
    StartOfDayBalance account -> [set account $ const amount]    
    Income account _ _ -> [set account (`plus` amount)]
    Expense account _ _ -> [set account (`minus` amount)]
    Transfer fromAccount toAccount -> [set fromAccount (`minus` amount),
                                       set toAccount (`plus` amount)]



