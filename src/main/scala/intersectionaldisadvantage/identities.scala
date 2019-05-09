package intersectionaldisadvantage

import intersectionaldisadvantage.Main.P1_PROPORTION
import intersectionaldisadvantage.Main.Q1_PROPORTION

trait IdentityComponent {
  def proportion: Double
}

// represents the two components of an identity
sealed trait P extends IdentityComponent

object P1 extends P {
  val proportion: Double = P1_PROPORTION

  override def toString: String = "P1"
}

object P2 extends P {
  val proportion: Double = 1 - Q1.proportion

  override def toString: String = "P2"
}

sealed trait Q extends IdentityComponent

object Q1 extends Q {
  val proportion: Double = Q1_PROPORTION

  override def toString: String = "Q1"
}

object Q2 extends Q {
  val proportion: Double = 1 - Q1.proportion

  override def toString: String = "Q2"
}