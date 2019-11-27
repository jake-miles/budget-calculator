package com.jm.budgetcalc.model

import java.util.Date

trait Money {

  val cents: Int
  def copy(cents : Int): Money

  def + (other:Money): Money = copy(this.cents + other.cents)
  def - (other:Money): Money = this + (-other)
  def unary_- : Money = copy(-cents)

  def dollars_and_cents: (Int, Int) =
    (this.cents / 100, this.cents % 100)
}

case class SignedMoney(cents: Int) extends Money

case class PositiveMoney(cents:Int) extends Money {
  assert(cents >= 0, s"PositiveMoney only accept a Money with cents >= 0. Was given $cents")
}

case class Account(name: String)
case class ExternalEntitity(name: String)

trait Transaction
case class Income(account: Account, who: Option[String], what: Option[String]) extends Transaction
case class Expense(account: Account, who: Option[String], what: Option[String]) extends Transaction
case class Transfer(from: Account, to: Account) extends Transaction {}
case class SetAccountBalance(account: Account) extends Transaction {}

case class LedgerEvent(date: Date, amount: PositiveMoney, transaction: Transaction)

case class Ledger(events: List[LedgerEvent]) {



}