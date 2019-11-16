module Ledger where

import Data.Time (Day)
import Data.List 
import Money

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
                 | StartOfDayBalance Account -- initial or balance override/correction
  deriving (Eq, Show)

applyTransaction :: Transaction -> PositiveMoney -> (Account -> Money) -> [(Account, Money)]
applyTransaction t positiveAmount amountBefore =
  case t of
    StartOfDayBalance account -> [update account $ const eventAmount]    
    Income account _ _ -> [update account (`plus` eventAmount)]
    Expense account _ _ -> [update account (`minus` eventAmount)]
    Transfer fromAccount toAccount -> [update fromAccount (`minus` eventAmount),
                                       update toAccount (`plus` eventAmount)]
  where
    eventAmount = toMoney positiveAmount
    update account calcNewAmount = (account, newAmount)
      where newAmount = calcNewAmount $ amountBefore account

applyEvent :: [Balance] -> Event -> [Balance]
applyEvent existingBalances (Event date amount transaction) =
  let

    existingBalance :: Account -> Maybe Balance
    existingBalance account =
      find ((==account) . balanceAccount) existingBalances

    amountBefore :: Account -> Money
    amountBefore =
      (withDefault zero) . (fmap balanceAmount . existingBalance)

    eventAmounts = applyTransaction transaction amount amountBefore
    eventAccounts = map fst eventAmounts
    isChanged =
      (`elem` eventAccounts) . balanceAccount

    toBalance (account, amount) =
      Balance date amount account

    eventBalances = map toBalance eventAmounts
    unchangedBalances = filter (not . isChanged) existingBalances
      
  in
    eventBalances ++ unchangedBalances

balanceHistory :: [Event] -> [Balance]
balanceHistory events =
  foldl' applyEvent [] $ sortOn eventDate events
  
withDefault :: a -> Maybe a -> a
withDefault a maybeA = maybe a id maybeA

    



