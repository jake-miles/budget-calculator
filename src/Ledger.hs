module Ledger where

import Data.Time
import Data.Sort
import Money

data Account = ExternalEntity String
             | Account String

data Balance = Balance {
  balanceAccount :: Account,
  balanceDay :: Day,
  balanceAmount :: Money
}

type SetBalance = Balance

data Transaction = SetBalance
                 | Transfer {
                     transactionDate :: Day,
                     accountFrom :: Account,
                     accountTo :: Account,
                     transactionAmount :: Money,
                     transactionReason :: Maybe[String]
                     }

applyToBalance :: Balance -> Transaction -> Balance
applyToBalance b@(Balance account _ balance) transaction =
  case transaction of

    newBalance@(SetBalance setBalanceAccount _ _) 
      | account == setBalanceAccount -> newBalance
      | _otherwise -> b

    transfer@(Transfer date from to delta)
      | from == account -> Balance account date (subtract balance delta)
      | to == account -> Balance account date (add balance delta)
      | _ -> b

applyTransaction :: [Balance] -> Transaction -> [Balance]
applyTransaction balances newInitialBalance =
  map applyToBalance balances
  
applyTransactions :: [Balance] -> [Transaction] -> [Balance]
applyTransactions balances transactions =
  foldl' applyTransaction balances $ sortBy transactionDate transactions

