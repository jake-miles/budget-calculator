package com.jm.budgetcalc.model

import java.util.Date

case class Account(name: String)
case class ExternalEntity(name: String)

case class Balance(account: Account, date: Date, amount: Money)

case class LedgerEvent[T <: Transaction](date: Date, amount: PositiveMoney, transaction: T)

trait Transaction
case class SetAccountBalance(account: Account) extends Transaction
case class Income(account: Account, who: Option[ExternalEntity], what: Option[String]) extends Transaction
case class Expense(account: Account, who: Option[ExternalEntity], what: Option[String]) extends Transaction
case class Transfer(from: Account, to: Account) extends Transaction

object Transaction {

  type ChangedAccounts = Map[Account, Money]
  type AccountLookup = Account => Money
  type TransactionOp[T <: Transaction] = (T, PositiveMoney, AccountLookup) => ChangedAccounts

  def applyEvent[T <: Transaction](event: LedgerEvent[T], before: List[Balance])
                                  (implicit transactionOp: TransactionOp[T]): List[Balance] = {

    def amountBefore(account: Account): Money =
      before
        .find(_.account == account)
        .map(_.amount)
        .getOrElse(Money.zero)

    val changedBalances =
      transactionOp.apply(event.transaction, event.amount, amountBefore)
                   .map { case (account, amount) => Balance(account, event.date, amount) }

    val changedAccounts = changedBalances.map(_.account).toSet

    val unchangedBalances =
      before.filter(b => ! changedAccounts.contains(b.account))

    unchangedBalances ++ changedBalances
  }

  implicit def applySetAccountBalance(set: SetAccountBalance,
                                      amount: PositiveMoney,
                                      amountBefore: AccountLookup): ChangedAccounts =
      Map(set.account -> amount)

  implicit def applyIncome(income: Income,
                           amount: PositiveMoney,
                           amountBefore: AccountLookup): ChangedAccounts =
      Map(income.account -> (amountBefore(income.account) + amount))

  implicit def applyExpense(expense: Expense,
                            amount: PositiveMoney,
                            amountBefore: AccountLookup): ChangedAccounts =
      Map(expense.account -> (amountBefore(expense.account) - amount))

  implicit def applyTransfer(transfer: Transfer,
                             amount: PositiveMoney,
                             amountBefore: AccountLookup): ChangedAccounts =
      Map(
        transfer.from -> (amountBefore(transfer.from) - amount),
        transfer.to -> (amountBefore(transfer.to) + amount)
      )
}