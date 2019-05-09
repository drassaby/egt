package intersectionaldisadvantage


object Main {
  val RUNS = 1000
  val MAX_GENERATIONS = 10000

  val PAYOFFS = PayoffMatrix(Vector(
    Vector((4, 4), (6, 4)),
    Vector((6, 4), (0, 0)),
  ))

  // assumes a game where both players have the same strategy set
  val NUM_STRATEGIES: Int = PAYOFFS.length

  val P1_PROPORTION, Q1_PROPORTION = .9

  def main(args: Array[String]): Unit = {
    val outcome = ModerateIntersectionalitySimulation(
      PAYOFFS,
      runs=RUNS,
      maxGenerations=MAX_GENERATIONS)

    var mostFrequentOut = Map[(P, Q), Vector[Int]]()


      // Record the results from this run
      outcome.foreach {
        _.foreach {
        case ((p: P, q: Q), strategy: Strategy) =>
          val newOut = mostFrequentOut.getOrElse((p, q), Vector()) :+ strategy.out.indexOf(strategy.out.max)
          mostFrequentOut = mostFrequentOut.updated((p, q), newOut)
      }
    }

    // Print the results
    println(mostFrequentOut.mapValues(_.groupBy(x => x).mapValues(_.length)))
  }

}


case class PayoffMatrix(payoffs: Vector[Vector[(Int, Int)]]) {
  // payoffs must be square and nonempty
  assert(payoffs.length == payoffs.head.length)

  def length: Int = payoffs.length

  def strategyPayoffs(player1: Vector[Double], p1prop: Double,
                      player2: Vector[Double], p2prop: Double
                     ): Vector[Double] = {
    (for (i <- payoffs.indices) yield {
      var strategyPayoff = 0d

      for (j <- payoffs.head.indices) {
        strategyPayoff +=
          // the proportion of the time that I play i against you playing j
          // multiplied by the payoff for i against j
          // multiplied by the proportion of the time we face off.
          (player2(j) * payoffs(i)(j)._1 * p1prop
            + player2(j) * payoffs(j)(i)._2 * p2prop)
      }

      strategyPayoff
    }).toVector
  }
}