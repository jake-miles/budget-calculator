module Ledger where

import Data.Time
import Data.List
import Money

data Account = ExternalEntity String
             | Account String
  deriving (Eq, Show)

data Balance = Balance {
  balanceAccount :: Account,
  balanceAmount :: Money
} deriving (Eq, Show)

data Transaction = BalanceOverride Day Balance
                 | Transfer {
                     transferDate :: Day,
                     transferFrom :: Account,
                     transferTo :: Account,
                     transferAmount :: Money,
                     transferReason :: String
                   }
  deriving (Eq, Show)

transactionDate :: Transaction -> Day
transactionDate (BalanceOverride date _) = date
transactionDate (Transfer date _ _ _ _) = date

applyTransaction :: Transaction -> [Balance] -> [Balance]
applyTransaction transaction =
  map (updateBalance transaction)

updateBalance :: Transaction -> Balance -> Balance
updateBalance transaction balance@(Balance account amount) =
  let
    noUpdate = balance    
    update newAmount = Balance account newAmount
  in
    case transaction of
       
      BalanceOverride _ (Balance (ExternalEntity _) _) ->
        error "You can only apply a BalanceOverride to an Account, not an ExternalEntity"
  
      Transfer _ (ExternalEntity _) (ExternalEntity _) _ _ ->
        error "One of a Transfer's accounts must be an Account; both cannot be ExternalEntities"
  
      BalanceOverride _ (Balance setAccount overrideAmount)
        | account == setAccount -> update overrideAmount
        | otherwise -> noUpdate

      Transfer date from to delta _
        | account == from -> update (Money.subtract amount delta)
        | account == to -> update (Money.add amount delta)
        | otherwise -> noUpdate

data Ledger = Ledger [Account] [Transaction]

ledger :: [Account] -> [Transaction] -> Ledger
ledger accounts transactions =
  Ledger accounts $ sortOn transactionDate transactions

ledgerBalances :: Ledger -> [Balance]
ledgerBalances (Ledger accounts transactions) =
  
  

--balancesAsOf :: Day -> [Transaction] -> [Balance]



