package com.jm.budgetcalc.model

trait Money {

  val cents: Int
  def copy(cents : Int): Money

  def + (other:Money): Money = this.copy(this.cents + other.cents)
  def - (other:Money): Money = this + (-other)
  def unary_- : Money = this.copy(-cents)

  def dollars_and_cents: (Int, Int) =
    (this.cents / 100, this.cents % 100)
}

case class SignedMoney(cents: Int) extends Money

/* `cents` is guaranteed to be >= 0 */
case class PositiveMoney(cents:Int) extends Money {
  assert(cents >= 0, s"PositiveMoney only accept a Money with cents >= 0. Was given $cents")
}

