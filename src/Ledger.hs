module Ledger where

import Data.Time
import Data.List
import Money

data Account = ExternalEntity String
             | Account String
  deriving (Eq, Show)

data Balance = Balance {
  balanceAccount :: Account,
  balanceDate :: Day,
  balanceAmount :: Money
} deriving (Eq, Show)

type From = Account
type To = Account
type Amount = Money
type Reason = String
data Transaction = SetBalance Balance 
                 | Transfer Day From To Amount Reason
  deriving (Eq, Show)

transactionDate :: Transaction -> Day
transactionDate (SetBalance (Balance _ date _ )) = date
transactionDate (Transfer date _ _ _ _) = date

updateBalance :: Transaction -> Balance -> Balance
updateBalance transaction before@(Balance account _ accountValue) =
  case transaction of

    -- note that `accountSet` cannot be an ExternalEntity
    SetBalance newBalance@(Balance accountSet@(Account _) _ _)
      | account == accountSet -> newBalance
      | otherwise -> before

    Transfer date from to delta _
      | account == from -> Balance account date (Money.subtract accountValue delta)
      | account == to -> Balance account date (Money.add accountValue delta)
      | otherwise -> before

applyTransaction :: [Balance] -> Transaction -> [Balance]
applyTransaction balances transaction =
  map (updateBalance transaction) balances

-- todo: how to make sure all Accounts referred to in the ledger exist?
-- SetBalance could lazy-add the account. 
  
--balancesAsOf :: Day -> [Transaction] -> [Balance]
