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

data Transaction = Transfer {
  transferAmount :: Money,  
  transferFrom :: Account,
  transferTo :: Account,
  transferDate :: Day,  
  transferReason :: String
}
  deriving (Eq, Show)

transfer :: Money -> Account -> Account -> Date -> String -> Transfer
transfer amount
  | amount > 0 = Transfer amount

class LedgerLine a where
  ledgerLineDate :: a -> Day
  updateBalance :: a -> Balance -> Balance

instance LedgerLine Balance where

  ledgerLineDate (Balance _ date _) = date
  
  updateBalance (Balance (ExternalEntity _) _) =
    error "You can only apply a BalanceOverride to an Account, not an ExternalEntity"

  updateBalance new@(Balance _ updatedAccount _) old@(Balance _ account _) 
    | updatedAccount == account = new
    | otherwise = old

instance LedgerLine Transaction where

  ledgerLineDate (Transfer _ _ date _ _) = date

  updateBalance (Transfer _ (ExternalEntity _) (ExternalEntity _) _ _) =
    error "One of a Transfer's accounts must be an Account; both cannot be ExternalEntities"

  updateBalance (Transfer amount from to date _) old@(Balance balance account _)
    | account == from = update (Money.negate amount)
    | account == to = update amount
    | otherwise = old
    where
      update delta = Balance (add balance delta) account date

data Ledger = Ledger [Balance] [LedgerLine]
  deriving (Eq, Show)

ledger :: [Balance] -> [LedgerLine] -> Ledger
ledger accounts transactions =
  Ledger accounts $ sortOn transactionDate transactions

ledgerBalances :: Ledger -> [Balance]
ledgerBalances (Ledger accounts transactions) =
  
  

--balancesAsOf :: Day -> [Transaction] -> [Balance]



