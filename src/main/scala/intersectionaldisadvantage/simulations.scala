package intersectionaldisadvantage

import scala.collection.mutable

trait TwoArenaSimulation extends ((PayoffMatrix, Int, Int) => Vector[Population]) {
  override def apply(payoffs: PayoffMatrix, runs: Int, maxGenerations: Int)
  : Vector[Map[(P, Q), Strategy]]
}


abstract class AbstractTwoArenaSimulation extends TwoArenaSimulation {
  // how close each vector of strategy probablities has to be to 1
  val REPLICATION_EPSILON = 0.001

  // how close does each corresponding element in two populations have to be for it to be stable
  val STABILITY_EPSILON = 0.0001


  override def apply(payoffs: PayoffMatrix, runs: Int, maxGenerations: Int)
  : Vector[Map[(P, Q), Strategy]] = {
    val strategiesAtTermination = mutable.ArrayBuffer[Population]()

    // assumes a game where both players have the same strategy set
    val numStrategies: Int = payoffs.length

    // Run the simulation RUNS times
    for (run <- 1 to runs) {
      var oldPop: Population = fillStrategies(numStrategies, Vector.fill(_)(0d))

      // Initialize the strategy vectors randomly, for each identity
      var newPop: Population = fillStrategies(numStrategies, randFill)
      var generation = 0

      while (!isStable(oldPop, newPop) && generation <= maxGenerations) {
        generation += 1
        oldPop = newPop
        newPop = replicate(payoffs, oldPop)
      }

      strategiesAtTermination += newPop
    }

    strategiesAtTermination.toVector
  }

  protected def replicate(payoffs: PayoffMatrix, population: Population): Population

  private def fillStrategies(numStrategies: Int, thunk: Int => Vector[Double]): Map[(P, Q), Strategy] = {
    Vector((P1, Q1), (P2, Q1), (P1, Q2), (P2, Q2))
      .map(_ -> Strategy(thunk(numStrategies), thunk(numStrategies)))
      .toMap
  }


  /**
    * @param n the length of the vector
    * @return a vector with samples from a uniform distribution that sum to one.
    */
  private def randFill(n: Int): Vector[Double] = {
    // The negative logarithm is needed to ensure an unbiased distribution
    // Seems a bit strange if you don't know about random-point-picking
    // in Simplexes, but trust me...
    val y = Vector.fill(n)(-math.log(util.Random.nextDouble()))
    y.map(_ / y.sum)
  }


  private def isStable(oldPop: Population, newPop: Population): Boolean = {
    def vectorStable(v1: Vector[Double], v2: Vector[Double]): Boolean = {
      v1.corresponds(v2)((e1, e2) => math.abs(e1 - e2) < STABILITY_EPSILON)
    }

    oldPop.keys.forall(k =>
      vectorStable(oldPop(k).in, newPop(k).in) && vectorStable(oldPop(k).out, newPop(k).out)
    )
  }

}

object ModerateIntersectionalitySimulation extends AbstractTwoArenaSimulation {
  override protected def replicate(payoffs: PayoffMatrix, population: Population): Population = {
    population.map {
      case ((p: P, q: Q), strategy: Strategy) =>
        // Update in-group strategy
        val prop = p.proportion * q.proportion
        val inGroupPayoffs =
          payoffs.strategyPayoffs(strategy.in, prop, strategy.in, prop)
        assert(inGroupPayoffs.length == payoffs.length)

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
        assert(outGroupPayoffs.length == payoffs.length)

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
}
