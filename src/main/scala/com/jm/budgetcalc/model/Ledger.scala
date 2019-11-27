package com.jm.budgetcalc.model

import java.util.Date

case class Account(name: String)
case class ExternalEntity(name: String)

case class Balance(account: Account, date: Date, amount: Money)

trait Transaction {
  def apply(amount: PositiveMoney,
            amountBefore: Account => Money): Map[Account, Money]
}

object Transaction {

  type ChangedBalances = Map[Account, Money]

  case class SetAccountBalance(account: Account) extends Transaction {
    def apply(amount: PositiveMoney, amountBefore: Account => Money): ChangedBalances =
      Map(account -> amount)
  }

  case class Income(account: Account, who: Option[ExternalEntity], what: Option[String]) extends Transaction {
    def apply(amount: PositiveMoney, amountBefore: Account => Money): ChangedBalances =
      Map(account -> (amountBefore(account) + amount))
  }

  case class Expense(account: Account, who: Option[ExternalEntity], what: Option[String]) extends Transaction {
    def apply(amount: PositiveMoney, amountBefore: Account => Money): ChangedBalances =
      Map(account -> (amountBefore(account) - amount))
  }

  case class Transfer(from: Account, to: Account) extends Transaction {
    def apply(amount: PositiveMoney, amountBefore: Account => Money): ChangedBalances =
      Map(
        from -> (amountBefore(from) - amount),
        to -> (amountBefore(to) + amount)
      )
  }

}

case class LedgerEvent(date: Date, amount: PositiveMoney, transaction: Transaction)

object LedgerOps {

  def applyEvent(event: LedgerEvent, before: List[Balance]): List[Balance] = {

    def amountBefore(account: Account) =
      before
        .find(_.account == account)
        .getOrElse(PositiveMoney(0))

    val changed: Map[Account, Money] = event.transaction(event.amount, amountBefore)

    val unchanged: List[Balance] =
      before.filter(balance => changed.get(balance.account).isEmpty)

    unchanged.concat(changed.values)
  }
}