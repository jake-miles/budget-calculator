package com.jm.budgetcalc.model

import java.util.Date

case class Account(name: String)
case class ExternalEntity(name: String)

case class Balance(account: Account, date: Date, amount: Money)

case class LedgerEvent(date: Date, amount: PositiveMoney, transaction: Transaction)

trait Transaction
case class SetAccountBalance(account: Account) extends Transaction
case class Income(account: Account, who: Option[ExternalEntity], what: Option[String]) extends Transaction
case class Expense(account: Account, who: Option[ExternalEntity], what: Option[String]) extends Transaction
case class Transfer(from: Account, to: Account) extends Transaction

object Transaction {

  def applyEvent(event: LedgerEvent, before: List[Balance]): List[Balance] = {

    def amountBefore(account: Account): Money =
      before
        .find(_.account == account)
        .map(_.amount)
        .getOrElse(Money.zero)

    val changedBalances =
      applyTransaction(event.transaction, event.amount, amountBefore)
        .map { case (account, amount) => Balance(account, event.date, amount) }

    val changedAccounts = changedBalances.map(_.account).toSet

    val unchangedBalances =
      before.filter(b => ! changedAccounts.contains(b.account))

    unchangedBalances ++ changedBalances
  }

  def applyTransaction(transaction: Transaction,
                       amount: PositiveMoney,
                       amountBefore: Account => Money): Map[Account, Money] = transaction match {
    case SetAccountBalance(account) => Map(account -> amount)
    case Income(account, _, _) =>  Map(account -> (amountBefore(account) + amount))
    case Expense(account, _, _) =>  Map(account -> (amountBefore(account) - amount))
    case Transfer(from, to) => Map(
      from -> (amountBefore(from) - amount),
      to -> (amountBefore(to) + amount)
    )
  }
}