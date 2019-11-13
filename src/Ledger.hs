module Ledger where

import Data.Time
import Data.List
import Money

data Account = ExternalEntity String
             | Account String
  deriving (Eq, Show)

data Balance = Balance {
  balanceAmount :: Money,
  balanceAccount :: Account,    
  balanceDate :: Day
} deriving (Eq, Show)

data Transaction = BalanceOverride Balance
                 | Transfer {
                     transferAmount :: Money,                     
                     transferFrom :: Account,
                     transferTo :: Account,
                     transferDate :: Day,                     
                     transferReason :: String
                   }
  deriving (Eq, Show)

data Ledger = Ledger [Balance] [Transaction]
  deriving (Show)

balance :: Money -> Account -> Day -> Balance
balance _ (ExternalEntity _) _ =
  error "You can only apply a BalanceOverride to an Account, not an ExternalEntity"

balance amount account date = Balance amount account date

transfer :: Money -> Account -> Account -> Day -> String -> Transaction
transfer _ (ExternalEntity _) (ExternalEntity _) _ _ =
  error "One of a Transfer's accounts must be an Account; both cannot be ExternalEntities"
  
transfer amount from to date reason
  | amount < Money(0) = error "You must specify a transfer as a positive amount"
  | otherwise = Transfer amount from to date reason

transactionDate :: Transaction -> Day
transactionDate (BalanceOverride (Balance _ _ date)) = date
transactionDate (Transfer _ _ _ date _) = date

applyTransaction :: Transaction -> [Balance] -> [Balance]
applyTransaction transaction =
  map (updateBalance transaction)

updateBalance :: Transaction -> Balance -> Balance

updateBalance transaction old@(Balance amount account _) =
  case transaction of
       
    BalanceOverride new@(Balance overrideAmount setAccount date)
      | account == setAccount -> new
      | otherwise -> old     

    Transfer amount from to date _
      | account == from -> update (Money.negate amount)
      | account == to -> update amount
      | otherwise -> old
      where
        update delta = Balance (Money.add amount delta) account date 

-- the initial balances must be sorted in with the Transactions, and
-- may not sort before them ... is something not quite right?
ledger :: [Balance] -> [Transaction] -> Ledger
ledger initial transactions =
  Ledger initial $ sortOn transactionDate transactions

-- ledgerBalances :: Ledger -> [Balance]
-- ledgerBalances (Ledger accounts transactions) =
  
  

--balancesAsOf :: Day -> [Transaction] -> [Balance]



