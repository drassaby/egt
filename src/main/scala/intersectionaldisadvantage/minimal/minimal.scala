package intersectionaldisadvantage.minimal

import intersectionaldisadvantage.{P, Q}

package object minimal {

  // a population can be seen as a mapping from each intersectional identity to a strategy.
  // because proportions of P1, P2, Q1, Q2 are fixed
  type Population = Map[(P, Q), Strategy]

  /** The proportion of games where each strategy is played against in and out groups. */
  case class Strategy(p: ArenaStrategy, q: ArenaStrategy)

  case class ArenaStrategy(in: IndexedSeq[Double], out: IndexedSeq[Double]) {
    // every strategy has to be represented in the in- and out- group strategy ratio vectors
    //    assert(in.length == PAYOFFS.length)
    //    assert(out.length == PAYOFFS.length)

    override def toString(): String = {
      f"""
         |Strategy[
         |  In: ${in.map("%.2f" format _).mkString(",")},
         |  Out: ${out.map("%.2f" format _).mkString(",")}]""".stripMargin
    }
  }

}
