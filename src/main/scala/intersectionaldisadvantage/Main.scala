package intersectionaldisadvantage


object Main {
  val RUNS = 1000
  val MAX_GENERATIONS = 1000

  val PAYOFFS = PayoffMatrix(Vector(
    Vector((4, 4), (6, 4)),
    Vector((6, 4), (0, 0)),
  ))

  // assumes a game where both players have the same strategy set
  val NUM_STRATEGIES: Int = PAYOFFS.length

  val P1_PROPORTION, Q1_PROPORTION = .8

  def main(args: Array[String]): Unit = {
    val outcome = MinimalIntersectionalitySimulation(
      PAYOFFS,
      runs = RUNS,
      maxGenerations = MAX_GENERATIONS)

    var pqHighFrequencies = Vector[(P, Q)]()

//    println(outcome)
    // Record the results from this run
    outcome.foreach {
      population => {
        println(population)
        val pHigh = if (population(P1, Q1).p.out.indexOf(population(P1, Q1).p.out.max) == 1) {
          P1
        } else {
          P2
        }

        val qHigh = if (population(P1, Q1).q.out.indexOf(population(P1, Q1).q.out.max) == 1) {
          Q1
        } else {
          Q2
        }
        println(pHigh, qHigh)
        pqHighFrequencies = pqHighFrequencies :+ (pHigh, qHigh)

      }

      //        _.foreach {
      //
      //        case ((p: P, q: Q), strategy: Strategy) =>
      //          pqHighFrequencies = pqHighFrequencies :+ (
      //            strategy.p.out.indexOf(strategy.p.out.max),
      //          strategy.q.out.indexOf(strategy.q.out.max))
      //      }
    }

    // Print the results
    //    println(mostFrequentPOut.mapValues(_.groupBy(x => x).mapValues(_.length)))
    //    println(mostFrequentQOut.mapValues(_.groupBy(x => x).mapValues(_.length)))
    println(pqHighFrequencies.groupBy(x => x).mapValues(_.length))

  }

}


case class PayoffMatrix(payoffs: Vector[Vector[(Int, Int)]]) {
  // payoffs must be square and nonempty
  assert(payoffs.length == payoffs.head.length)

  def length: Int = payoffs.length


  def strategyPayoffs(player1: Vector[Double], p1prop: Double): Vector[Double] = {
    twoPopulationStrategyPayoffs(player1, p1prop, player1, p1prop)
  }

  def twoPopulationStrategyPayoffs(player1: Vector[Double], p1prop: Double,
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