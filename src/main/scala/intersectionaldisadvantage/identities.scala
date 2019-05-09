package intersectionaldisadvantage

import intersectionaldisadvantage.Main.P1_PROPORTION
import intersectionaldisadvantage.Main.Q1_PROPORTION

trait IdentityComponent {
  def proportion: Double
}

// represents the two components of an identity
sealed trait P extends IdentityComponent {
  def opposite: P
}

object P1 extends P {
  val proportion: Double = P1_PROPORTION

  override def toString: String = "P1"

  def opposite: P = P2
}

object P2 extends P {
  val proportion: Double = 1 - Q1.proportion

  override def toString: String = "P2"
  def opposite: P = P1
}

sealed trait Q extends IdentityComponent {
  def opposite: Q
}

object Q1 extends Q {
  val proportion: Double = Q1_PROPORTION

  override def toString: String = "Q1"
  def opposite: Q = Q2
}

object Q2 extends Q {
  val proportion: Double = 1 - Q1.proportion

  override def toString: String = "Q2"
  def opposite: Q = Q1
}