package intersectionaldisadvantage

import intersectionaldisadvantage.Main.Strategy

object Main {
  val RUNS = 100
  val MAX_GENERATIONS = 10000
  val STABILITY_EPSILON = 0.0001
  val REPLICATION_EPSILON = 0.1

  val PAYOFFS = PayoffMatrix(Vector(
    Vector((1, 1), (1, 9)),
    Vector((9, 1), (0, 0)),
  ))

  val P1_PROPORTION = .8
  val Q1_PROPORTION = .8

  trait IdentityComponent {
    def proportion: Double
  }

  // represents the two components of an identity
  sealed trait P extends IdentityComponent

  object P1 extends P {
    val proportion: Double = .5

    override def toString: String = "P1"
  }

  object P2 extends P {
    val proportion: Double = 1 - Q1.proportion

    override def toString: String = "P2"
  }

  sealed trait Q extends IdentityComponent

  object Q1 extends Q {
    val proportion: Double = .5

    override def toString: String = "Q1"
  }

  object Q2 extends Q {
    val proportion: Double = 1 - Q1.proportion

    override def toString: String = "Q2"
  }

  // a population can be seen as a mapping from each intersectional identity to a strategy.
  // because proportions of P1, P2, Q1, Q2 are fixed
  type Population = Map[(P, Q), Strategy]

  /** The proportion of games where each strategy is played against in and out groups. */
  case class Strategy(in: Vector[Double], out: Vector[Double]) {
    // every strategy has to be represented in the in- and out- group strategy ratio vectors
    assert(in.length == PAYOFFS.length)
    assert(out.length == PAYOFFS.length)

    override def toString(): String = {
      f"""
         |Strategy[
         |  In: ${in.map("%.2f" format _).mkString(",")},
         |  Out: ${out.map("%.2f" format _).mkString(",")}]""".stripMargin
    }
  }

  // assumes a game where both players have the same strategy set
  val NUM_STRATEGIES: Int = PAYOFFS.length


  def main(args: Array[String]): Unit = {
    var p1q1InConvergences = Vector[Int]()
    var p1q1OutConvergences = Vector[Int]()

    // Run the simulation RUNS times
    for (run <- 1 to RUNS) {
      // TODO reduce this code repetition
      var oldPop: Population = Map(
        (P1, Q1) -> Strategy(Vector.fill(NUM_STRATEGIES)(0), Vector.fill(NUM_STRATEGIES)(0)),
        (P2, Q1) -> Strategy(Vector.fill(NUM_STRATEGIES)(0), Vector.fill(NUM_STRATEGIES)(0)),
        (P1, Q2) -> Strategy(Vector.fill(NUM_STRATEGIES)(0), Vector.fill(NUM_STRATEGIES)(0)),
        (P2, Q2) -> Strategy(Vector.fill(NUM_STRATEGIES)(0), Vector.fill(NUM_STRATEGIES)(0)),
      )

      // Initialize the strategy vectors randomly, for each identity
      var newPop: Population = Map(
        (P1, Q1) -> Strategy(randFill(NUM_STRATEGIES), randFill(NUM_STRATEGIES)),
        (P2, Q1) -> Strategy(randFill(NUM_STRATEGIES), randFill(NUM_STRATEGIES)),
        (P1, Q2) -> Strategy(randFill(NUM_STRATEGIES), randFill(NUM_STRATEGIES)),
        (P2, Q2) -> Strategy(randFill(NUM_STRATEGIES), randFill(NUM_STRATEGIES)),
      )
      var generation = 0

      while (!isStable(oldPop, newPop) && generation <= MAX_GENERATIONS) {
        generation += 1
        oldPop = newPop
        newPop = replicate(PAYOFFS, oldPop)
      }

      // Record the results from this run
      println(newPop)
      val inStrat = newPop((P1, Q1)).in
      p1q1InConvergences = p1q1InConvergences :+ inStrat.indexOf(inStrat.max)
      val outStrat = newPop((P1, Q1)).out
      p1q1OutConvergences = p1q1OutConvergences :+ outStrat.indexOf(outStrat.max)
    }

    // Print the results
    println(p1q1InConvergences.groupBy(x=>x).mapValues(_.length))
    println(p1q1OutConvergences.groupBy(x=>x).mapValues(_.length))
  }

  def replicate(payoffs: PayoffMatrix, population: Population): Population = {

    population.map {
      case ((p: P, q: Q), strategy: Strategy) =>
        // Update in-group strategy
        val prop = p.proportion * q.proportion
        val inGroupPayoffs =
          payoffs.strategyPayoffs(strategy.in, prop, strategy.in, prop)
        assert(inGroupPayoffs.length == NUM_STRATEGIES)

        val averageInGroupFitness = strategy.in.zip(inGroupPayoffs).map {
          case (proportion, payoff) => proportion * payoff
        }.sum

        // compute the new proportions of each strategy
        val newInGroupStrategy = strategy.in.zip(inGroupPayoffs).map {
          case (proportion, payoff) =>
            proportion * (payoff / averageInGroupFitness)
        }

        assert(math.abs(newInGroupStrategy.sum - 1) < REPLICATION_EPSILON)


        // Update out-group strategy
        val outGroupPayoffs = {
          // we have a vector of vectors of the proportion in each group of each strategy,
          // take the row-wise sum
          val outGroupStrategyProportions = (population - ((p, q)))
            .map {
              case ((otherP, otherQ), otherStrategy) =>
                otherStrategy.out.map(_ * otherP.proportion * otherQ.proportion)
            }.transpose.map(_.sum).toVector

          payoffs.strategyPayoffs(strategy.out, prop, outGroupStrategyProportions, 1 - prop)
        }
        assert(outGroupPayoffs.length == NUM_STRATEGIES)

        val averageOutGroupFitness = strategy.out.zip(outGroupPayoffs).map {
          case (proportion, payoff) => proportion * payoff
        }.sum

        // compute the new proportions of each strategy
        val newOutGroupStrategy = strategy.out.zip(outGroupPayoffs).map {
          case (proportion, payoff) =>
            proportion * (payoff / averageOutGroupFitness)
        }
        assert(math.abs(newOutGroupStrategy.sum - 1) < REPLICATION_EPSILON)

        (p, q) -> Strategy(newInGroupStrategy, newOutGroupStrategy)
    }
  }


  /**
    * @param n the length of the vector
    * @return a vector with samples from a uniform distribution that sum to one.
    */
  def randFill(n: Int): Vector[Double] = {
    // The negative logarithm is needed to ensure an unbiased distribution
    // Seems a bit strange if you don't know about random-point-picking
    // in Simplexes, but trust me...
    val y = Vector.fill(n)(-math.log(util.Random.nextDouble()))
    y.map(_ / y.sum)
  }


  def isStable(oldPop: Population, newPop: Population): Boolean = {
    def vectorStable(v1: Vector[Double], v2: Vector[Double]): Boolean = {
      v1.corresponds(v2)((e1, e2) => math.abs(e1 - e2) < STABILITY_EPSILON)
    }

    oldPop.keys.forall(k =>
      vectorStable(oldPop(k).in, newPop(k).in) && vectorStable(oldPop(k).out, newPop(k).out)
    )
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