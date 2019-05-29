package intersectionaldisadvantage

import scala.collection.mutable

package object moderate {
  type Population = Map[(P, Q), StrategyProportions]

  case class Strategy(pIn: Int, pOut: Int, qIn: Int, qOut: Int)

  case class StrategyVector(pIn: Vector[Double],
                            pOut: Vector[Double],
                            qIn: Vector[Double],
                            qOut: Vector[Double]) {
    assert(pIn.length == Main.strategies.length)
    assert(pIn.length == pOut.length && pOut.length == qIn.length && qIn.length == qOut.length)
  }

  /**
    *
    * @param proportions how often each strategy is played
    */
  case class StrategyProportions(proportions: Vector[Double], strategies: Vector[Strategy]) {
    assert(math.abs(proportions.sum - 1d) < REPLICATION_EPSILON)
    assert(proportions.length == strategies.length)

    /**
      * In each arena, what is the proportion of each strategy being played?
      *
      * @return
      */
    def strategyVector: StrategyVector = {
      val out = mutable.ArrayBuffer.fill(4)(mutable.ArrayBuffer.fill(Main.strategies.length)(0d))

      proportions.zip(strategies).foreach {
        case (proportion, strategy) =>
          out(0)(strategy.pIn) += proportion
          out(1)(strategy.pOut) += proportion
          out(2)(strategy.qIn) += proportion
          out(3)(strategy.qOut) += proportion
      }

      for (item <- out) {
        assert (math.abs(item.sum - 1d) < REPLICATION_EPSILON)
      }

      StrategyVector(out(0).toVector, out(1).toVector, out(2).toVector, out(3).toVector)
    }
  }
}
