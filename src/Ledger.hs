module Ledger where

import Data.Time (Day)
import Data.List 
import Money
import PositiveMoney

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

type What = String
type FromAccount = Account
type ToAccount = Account

data Transaction = Income Account (Maybe ExternalEntity) (Maybe What)
                 | Expense Account (Maybe ExternalEntity) (Maybe What)
                 | Transfer FromAccount ToAccount
                 | StartOfDayBalance Account
  deriving (Eq, Show)

balanceHistory :: [Event] -> [Balance]
balanceHistory events =
  foldl' applyEvent [] $ sortOn eventDate events

applyEvent :: [Balance] -> Event -> [Balance]
applyEvent existingBalances event =
  let

    existingBalance account =
      find ((==account) . balanceAccount) existingBalances

    toBalance (account, amount) =
      Balance (eventDate event) amount account      

    amountBeforeEvent :: Account -> Money
    amountBeforeEvent =
      (maybe zero id) . (fmap balanceAmount . existingBalance)

    eventBalances = map toBalance $ amountsAfterEvent event amountBeforeEvent
    
    unaffectedBalances =
      let existingAccounts = map balanceAccount existingBalances
      in filter ((`elem` existingAccounts) . balanceAccount) eventBalances
      
  in
    unaffectedBalances ++ eventBalances
    
amountsAfterEvent :: Event -> (Account -> Money) -> [(Account, Money)]
amountsAfterEvent (Event date positiveAmount transaction) amountBeforeEvent =
  let
    eventAmount = toMoney positiveAmount
    update account calcNewAmount = (account, newAmount)
      where newAmount = calcNewAmount $ amountBeforeEvent account
  in case transaction of
    StartOfDayBalance account -> [update account $ const eventAmount]    
    Income account _ _ -> [update account (`plus` eventAmount)]
    Expense account _ _ -> [update account (`minus` eventAmount)]
    Transfer fromAccount toAccount -> [update fromAccount (`minus` eventAmount),
                                       update toAccount (`plus` eventAmount)]



