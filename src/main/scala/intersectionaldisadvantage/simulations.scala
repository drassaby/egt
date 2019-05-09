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
    population.map {
      case ((p: P, q: Q), strategy: Strategy) =>
        
        val pArenaStrategy = {
          val pInGroupStrategy = strategy.p.in.map(_ * q.proportion)
            .zip(population((p, q.opposite)).p.in.map(_ * q.opposite.proportion)).map {
            case (s1,s2) => s1 + s2
          }

          val pInGroupPayoffs =
            payoffs.strategyPayoffs(pInGroupStrategy, p.proportion)
          assert(pInGroupPayoffs.length == payoffs.length)

          val averageInGroupFitness = strategy.p.in.zip(pInGroupPayoffs).map {
            case (proportion, payoff) => proportion * payoff
          }.sum

          // compute the new proportions of each strategy
          val newPInGroupStrategy = strategy.p.in.zip(pInGroupPayoffs).map {
            case (proportion, payoff) =>
              proportion * (payoff / averageInGroupFitness)
          }

          assert(math.abs(newPInGroupStrategy.sum - 1) < REPLICATION_EPSILON)


          // Update out-group strategy
          val outGroupPayoffs = {

            val pOutGroupStrategy = strategy.p.out.map(_ * q.proportion)
              .zip(population((p, q.opposite)).p.out.map(_ * q.opposite.proportion)).map {
              case (s1,s2) => s1 + s2
            }

            // we have a vector of vectors of the proportion in each group of each strategy,
            // take the row-wise sum
            val pOppositeOutGroupStrategy = population((p.opposite, q)).p.out.map(_ * q.opposite.proportion)
              .zip(population((p.opposite, q.opposite)).p.out.map(_ * q.opposite.proportion)).map {
              case (s1,s2) => s1 + s2
            }

            payoffs.twoPopulationStrategyPayoffs(
              pOutGroupStrategy, p.proportion,
              pOppositeOutGroupStrategy, p.opposite.proportion)
          }
          assert(outGroupPayoffs.length == payoffs.length)

          val averagePOutGroupFitness = strategy.p.out.zip(outGroupPayoffs).map {
            case (proportion, payoff) => proportion * payoff
          }.sum

          // compute the new proportions of each strategy
          val newPOutGroupStrategy = strategy.p.out.zip(outGroupPayoffs).map {
            case (proportion, payoff) =>
              proportion * (payoff / averagePOutGroupFitness)
          }
          assert(math.abs(newPOutGroupStrategy.sum - 1) < REPLICATION_EPSILON)

          ArenaStrategy(newPInGroupStrategy, newPOutGroupStrategy)
        }

        val qArenaStrategy = {
          val qInGroupStrategy = strategy.q.in.map(_ * p.proportion)
            .zip(population((p.opposite, q)).q.in.map(_ * p.opposite.proportion)).map {
            case (s1,s2) => s1 + s2
          }

          val qInGroupPayoffs =
            payoffs.strategyPayoffs(qInGroupStrategy, q.proportion)
          assert(qInGroupPayoffs.length == payoffs.length)

          val averageQInGroupFitness = strategy.q.in.zip(qInGroupPayoffs).map {
            case (proportion, payoff) => proportion * payoff
          }.sum

          // compute the new proportions of each strategy
          val newQInGroupStrategy = strategy.q.in.zip(qInGroupPayoffs).map {
            case (proportion, payoff) =>
              proportion * (payoff / averageQInGroupFitness)
          }

          assert(math.abs(newQInGroupStrategy.sum - 1) < REPLICATION_EPSILON)


          // Update out-group strategy
          val outGroupPayoffs = {
              val qOutGroupStrategy = strategy.q.out.map(_ * p.proportion)
                .zip(population((p.opposite, q)).q.out.map(_ * p.opposite.proportion)).map {
                case (s1,s2) => s1 + s2
              }

              // we have a vector of vectors of the proportion in each group of each strategy,
              // take the row-wise sum
              val qOppositeOutGroupStrategy = population((p, q.opposite)).q.out.map(_ * p.opposite.proportion)
                .zip(population((p.opposite, q.opposite)).p.out.map(_ * q.opposite.proportion)).map {
                case (s1,s2) => s1 + s2
              }

              payoffs.twoPopulationStrategyPayoffs(
                qOutGroupStrategy, p.proportion,
                qOppositeOutGroupStrategy, p.opposite.proportion)
            }
          assert(outGroupPayoffs.length == payoffs.length)

          val averageQOutGroupFitness = strategy.q.out.zip(outGroupPayoffs).map {
            case (proportion, payoff) => proportion * payoff
          }.sum

          // compute the new proportions of each strategy
          val newQOutGroupStrategy = strategy.q.out.zip(outGroupPayoffs).map {
            case (proportion, payoff) =>
              proportion * (payoff / averageQOutGroupFitness)
          }
          assert(math.abs(newQOutGroupStrategy.sum - 1) < REPLICATION_EPSILON)

          ArenaStrategy(newQInGroupStrategy, newQOutGroupStrategy)
        }
        
        (p, q) -> Strategy(pArenaStrategy,qArenaStrategy) 
    }
  }
}
