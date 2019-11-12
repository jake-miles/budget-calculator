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

type BalanceOverride = Balance

data Transaction = BalanceOverride
                 | Transaction {
                     transactionDate :: Day,
                     accountFrom :: Account,
                     accountTo :: Account,
                     transactionAmount :: Money,
                     transactionReason :: Maybe[String]
                     }

applyTransaction :: [Balance] -> Transaction -> [Balance]

applyTransaction balances newInitialBalance@(BalanceOverride account day amount) =
  map transform balances
  where
    transform b@(Balance _account _ _)
      | account == _account = newInitialBalance
      | otherwise = b

applyTransaction balances (Transaction date from to delta reason) =
  map transform balances
  where
    transform b@(Balance account _ balance)
      | from == account = Balance account date (subtract balance delta)
      | to == account = Balance account date (add balance delta)
      | otherwise = b

applyTransactions :: [Balance] -> [Transaction] -> [Balance]
applyTransactions balances transactions =
  foldl' applyTransaction balances $ sortBy transactionDate transactions

