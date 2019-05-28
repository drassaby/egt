package intersectionaldisadvantage

package object moderate {
  type Population = Map[(P, Q), StrategyProportions]

  case class Strategy(pIn: Int, pOut: Int, qIn: Int, qOut: Int)


  case class StrategyProportions(proportions: Vector[Double]) {
    assert(proportions.length == 16)

  }
}
