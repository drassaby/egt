package intersectionaldisadvantage

package object moderate {
  type Population = Map[(P, Q), StrategyProportions]

  case class Strategy(pIn: Int, pOut: Int, qIn: Int, qOut: Int)

  case class StrategyVector(pIn: IndexedSeq[Double],
                            pOut: IndexedSeq[Double],
                            qIn: IndexedSeq[Double],
                            qOut: IndexedSeq[Double]) {
    //    assert(pIn.length == Main.strategies.length)
    //    assert(pIn.length == pOut.length && pOut.length == qIn.length && qIn.length == qOut.length)
  }

  /**
    *
    * @param proportions how often each strategy is played
    */
  case class StrategyProportions(proportions: IndexedSeq[Double], strategies: IndexedSeq[Strategy]) {
    assert(math.abs(proportions.sum - 1d) < REPLICATION_EPSILON)
    assert(proportions.length == strategies.length)

    /**
      * In each arena, what is the proportion of each strategy being played?
      *
      * @return
      */
    def strategyVector: StrategyVector = {
      val out = Array.fill(4)(Array.fill(Main.strategies.length)(0d))

      for (i <- proportions.indices) {
        val proportion = proportions(i)
        val strategy = strategies(i)
        out(0)(strategy.pIn) += proportion
        out(1)(strategy.pOut) += proportion
        out(2)(strategy.qIn) += proportion
        out(3)(strategy.qOut) += proportion
      }

      StrategyVector(out(0), out(1), out(2), out(3))
    }
  }
}
