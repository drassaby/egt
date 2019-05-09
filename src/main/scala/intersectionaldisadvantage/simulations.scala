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
      .map(_ -> Strategy(
        ArenaStrategy(thunk(numStrategies), thunk(numStrategies)),
        ArenaStrategy(thunk(numStrategies), thunk(numStrategies))))
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
      vectorStable(oldPop(k).p.in, newPop(k).p.in) &&
        vectorStable(oldPop(k).p.out, newPop(k).p.out) &&
        vectorStable(oldPop(k).q.in, newPop(k).q.in) &&
        vectorStable(oldPop(k).q.out, newPop(k).q.out)
    )
  }

}

object MinimalIntersectionalitySimulation extends AbstractTwoArenaSimulation {
  override protected def replicate(payoffs: PayoffMatrix, population: Population): Population = {
    // TODO there should be some way to remove duplication between arenas
    def runArena(salientIdentity: IdentityComponent,
                 secondaryIdentity: IdentityComponent,
                 retrieveStrategy: Strategy => ArenaStrategy)
    : ArenaStrategy = {

      def getStrategy(i1: IdentityComponent, i2: IdentityComponent): ArenaStrategy = {
        val (p, q) = (i1, i2) match {
          case (p: P, q: Q) => (p, q)
          case (q: Q, p: P) => (p, q)
          case _ => throw new IllegalArgumentException()
        }

        retrieveStrategy(population((p, q)))
      }

      val newInGroupStrategy: Vector[Double] = {
        val strategy: ArenaStrategy = getStrategy(salientIdentity, secondaryIdentity)

        val inGroupStrategy = strategy.in.map(_ * salientIdentity.proportion)
          .zip(getStrategy(salientIdentity, secondaryIdentity.opposite).in
            .map(_ * salientIdentity.opposite.proportion))
          .map {
            case (s1, s2) => s1 + s2
          }

        val inGroupPayoffs: Vector[Double] =
          payoffs.strategyPayoffs(inGroupStrategy, salientIdentity.proportion)
        assert(inGroupPayoffs.length == payoffs.length)

        val averageInGroupFitness: Double = strategy.in.zip(inGroupPayoffs).map {
          case (proportion, payoff) => proportion * payoff
        }.sum

        // compute the new proportions of each strategy
        strategy.in.zip(inGroupPayoffs).map {
          case (proportion, payoff) =>
            proportion * (payoff / averageInGroupFitness)
        }
      }
      assert(math.abs(newInGroupStrategy.sum - 1) < REPLICATION_EPSILON)

      val newOutGroupStrategy: Vector[Double] = {

        val strategy = getStrategy(salientIdentity, secondaryIdentity).out

        // the strategy played by your salient identity against out-groups by that identity
        // is the weighted elementwise of two vectors of the groups in your in-group
        val outGroupStrategy = strategy.map(_ * secondaryIdentity.proportion)
          .zip(getStrategy(salientIdentity, secondaryIdentity.opposite).out
            .map(_ * secondaryIdentity.opposite.proportion)).map {
          case (s1, s2) => s1 + s2
        }

        // the strategy played against your salient identity by out-groups of that identity
        // we have a vector of vectors of the proportion in each group of each strategy,
        // take the row-wise sum
        val oppositeOutGroupStrategy =
        getStrategy(salientIdentity.opposite, secondaryIdentity).out
          .map(_ * secondaryIdentity.proportion)
          .zip(getStrategy(salientIdentity.opposite, secondaryIdentity.opposite).out
            .map(_ * secondaryIdentity.opposite.proportion)).map {
          case (s1, s2) => s1 + s2
        }

        val outGroupPayoffs = payoffs.twoPopulationStrategyPayoffs(
          outGroupStrategy, salientIdentity.proportion,
          oppositeOutGroupStrategy, salientIdentity.opposite.proportion)

        assert(outGroupPayoffs.length == payoffs.length)

        val averageOutGroupFitness = strategy.zip(outGroupPayoffs).map {
          case (proportion, payoff) => proportion * payoff
        }.sum

        strategy.zip(outGroupPayoffs).map {
          case (proportion, payoff) =>
            proportion * (payoff / averageOutGroupFitness)
        }
      }
      assert(math.abs(newOutGroupStrategy.sum - 1) < REPLICATION_EPSILON)

      ArenaStrategy(newInGroupStrategy, newOutGroupStrategy)
    }


    population.map {
      case ((p: P, q: Q), strategy: Strategy) =>
        (p, q) -> Strategy(runArena(p, q, _.p), runArena(q, p, _.q))
    }
  }

}
