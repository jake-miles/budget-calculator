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
  map transformBalance balances
  where
    transformBalance balance@(Balance _account _day _amount)
      | _account == account = newInitialBalance
      | otherwise = balance

applyTransaction balances (Transaction date from to delta reason) =
  map transformBalance balances
  where
    transformBalance balance@(Balance account day balanceAmount) 
      | account == from = (Balance account day (subtract balanceAmount delta))
      | account == to = (Balance account day (add balanceAmount delta))
      | otherwise = balance
      
applyTransactions :: [Balance] -> [Transaction] -> [Balance]
applyTransactions balances transactions =
  foldl' applyTransaction balances $ sortBy transactionDate transactions


      
