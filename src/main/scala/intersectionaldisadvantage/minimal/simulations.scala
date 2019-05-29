package intersectionaldisadvantage.minimal

import intersectionaldisadvantage._
import intersectionaldisadvantage.minimal.minimal.{ArenaStrategy, Population, Strategy}

import scala.collection.mutable


abstract class AbstractTwoArenaSimulation extends TwoArenaSimulation {

  override def apply(payoffs: Map[(Arena, P), PayoffMatrix], runs: Int, maxGenerations: Int)
  : Vector[(Int, Int)] = {
    val strategiesAtTermination = mutable.ArrayBuffer[Population]()

    // assumes a game where both players have the same strategy set
    val numStrategies: Int = payoffs.toVector.head._2.length

    // Run the simulation RUNS times
    for (run <- 1 to runs) {
      if (run % 100 == 0) {
        println(f"Ran ${run} runs")
      }
      var oldPop: Population = fillStrategies(numStrategies, Vector.fill(_)(0d))

      // Initialize the strategy vectors randomly, for each identity
      var newPop: Population = fillStrategies(numStrategies, Utils.randFill)
      var generation = 0

      while (!isStable(oldPop, newPop) && generation <= maxGenerations) {
        generation += 1
        oldPop = newPop
        newPop = replicate(payoffs, oldPop)
      }
      if (generation >= maxGenerations - 1) {
        println("Hit generation cap")
      }

      strategiesAtTermination += newPop
    }

    val outcome = strategiesAtTermination.toVector
    var pqHighFrequencies = Vector[(Int, Int)]()

    //    println(outcome)
    // Record the results from this run
    outcome.foreach {
      population => {
        def maxIndex(vector: Vector[Double]) = {
          vector.indexOf(vector.max)
        }

        println(population.take(100))
        val pMax = maxIndex(population(P1, Q1).p.out)

        val qMax = maxIndex(population(P1, Q1).q.out)
        pqHighFrequencies = pqHighFrequencies :+ (pMax, qMax)
      }
    }

    pqHighFrequencies
  }


  private def fillStrategies(numStrategies: Int, thunk: Int => Vector[Double]): Map[(P, Q), Strategy] = {
    Vector((P1, Q1), (P2, Q1), (P1, Q2), (P2, Q2))
      .map(_ -> Strategy(
        ArenaStrategy(thunk(numStrategies), thunk(numStrategies)),
        ArenaStrategy(thunk(numStrategies), thunk(numStrategies))))
      .toMap
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

  def findPAndQ(i1: IdentityComponent, i2: IdentityComponent): (P, Q) = {
    (i1, i2) match {
      case (p: P, q: Q) => (p, q)
      case (q: Q, p: P) => (p, q)
      case _ => throw new IllegalArgumentException()
    }
  }


  def makeStrategyRetriever(arena: Arena, population: Population)
                           (i1: IdentityComponent, i2: IdentityComponent): ArenaStrategy = {
    val (p, q) = findPAndQ(i1, i2)
    arena.strategy(population((p, q)))
  }


  def replicate(payoffs: Map[(Arena, P), PayoffMatrix], population: Population): Population = {
    population.map {
      case ((p: P, q: Q), strategy: Strategy) =>
        val arenaRunner = runArena(payoffs, population) _
        (p, q) -> Strategy(arenaRunner(p, q, PArena), arenaRunner(q, p, QArena))
    }
  }

  def runArena(payoffs: Map[(Arena, P), PayoffMatrix], population: Population)
              (salientIdentity: IdentityComponent,
               secondaryIdentity: IdentityComponent,
               arena: Arena)
  : ArenaStrategy
}

object MinimalIntersectionalitySimulation extends AbstractTwoArenaSimulation {
  override def toString(): String = "Minimal"

  def runArena(payoffMap: Map[(Arena, P), PayoffMatrix], population: Population)
              (salientIdentity: IdentityComponent,
               secondaryIdentity: IdentityComponent,
               arena: Arena)
  : ArenaStrategy = {
    val payoffs = payoffMap(arena, findPAndQ(salientIdentity, secondaryIdentity)._1)

    val getStrategy = makeStrategyRetriever(arena, population) _

    val strategyIn: Vector[Double] = getStrategy(salientIdentity, secondaryIdentity).in

    val inGroupStrategy = Utils.weightedElementwiseSum(
      strategyIn, secondaryIdentity.proportion,
      getStrategy(salientIdentity, secondaryIdentity.opposite).in,
      secondaryIdentity.opposite.proportion)

    val inGroupPayoffs: Vector[Double] =
      payoffs.strategyPayoffs(inGroupStrategy, salientIdentity.proportion)
    assert(inGroupPayoffs.length == payoffs.length)

    val strategyOut: Vector[Double] = getStrategy(salientIdentity, secondaryIdentity).out
    // the strategy played by your salient identity against out-groups by that identity
    // is the weighted elementwise sum of two vectors of the groups in your in-group
    val outGroupStrategy = Utils.weightedElementwiseSum(
      strategyOut,
      secondaryIdentity.proportion,
      getStrategy(salientIdentity, secondaryIdentity.opposite).out,
      secondaryIdentity.opposite.proportion)

    // the strategy played against your salient identity by out-groups of that identity
    // we have a vector of vectors of the proportion in each group of each strategy,
    // take the row-wise sum
    val oppositeOutGroupStrategy = Utils.weightedElementwiseSum(
      getStrategy(salientIdentity.opposite, secondaryIdentity).out, secondaryIdentity.proportion,
      getStrategy(salientIdentity.opposite, secondaryIdentity.opposite).out,
      secondaryIdentity.opposite.proportion)

    val outGroupPayoffs = payoffs.twoPopulationStrategyPayoffs(
      outGroupStrategy, salientIdentity.proportion,
      oppositeOutGroupStrategy, salientIdentity.opposite.proportion)

    assert(outGroupPayoffs.length == payoffs.length)
    /**
      * Indexed by (in group strategy)(out group strategy) leads to the fitness for playing
      * that in and out group strategy.
      */
    val jointFitness: Vector[Vector[Double]] = inGroupPayoffs.map(igp =>
      outGroupPayoffs.map(igp * salientIdentity.proportion +
        _ * salientIdentity.opposite.proportion))

    // compute the new proportions of each strategy
    val newInGroupStrategy: Vector[Double] = inGroupStrategy
      .zip(jointFitness.map(_.sum)).map {
      case (proportion, payoff) =>
        proportion * (payoff / Utils.dotProduct(inGroupStrategy, jointFitness.map(_.sum)))
    }

    val newOutGroupStrategy: Vector[Double] = outGroupStrategy
      .zip(jointFitness.transpose.map(_.sum)).map {
      case (proportion, payoff) =>
        proportion * (payoff / Utils.dotProduct(
          outGroupStrategy,
          jointFitness.transpose.map(_.sum)))
    }

    assert(math.abs(newInGroupStrategy.sum - 1) < REPLICATION_EPSILON)
    assert(math.abs(newOutGroupStrategy.sum - 1) < REPLICATION_EPSILON)

    ArenaStrategy(newInGroupStrategy, newOutGroupStrategy)
  }
}


//object ModerateIntersectionalitySimulation extends AbstractTwoArenaSimulation {
//  override def runArena(payoffs: PayoffMatrix, population: Population)
//                       (salientIdentity: IdentityComponent,
//                        secondaryIdentity: IdentityComponent,
//                        retrieveStrategy: Strategy => ArenaStrategy)
//  : ArenaStrategy = {
//    val getStrategy = makeStrategyRetriever(retrieveStrategy, population) _
//
//    val strategyIn: Vector[Double] = getStrategy(salientIdentity, secondaryIdentity).in
//
//    val inGroupStrategy = Utils.weightedSum(
//      strategyIn,
//      secondaryIdentity.proportion,
//      getStrategy(salientIdentity, secondaryIdentity.opposite).in,
//      secondaryIdentity.opposite.proportion)
//
//    val inGroupPayoffs: Vector[Double] =
//      payoffs.strategyPayoffs(inGroupStrategy, salientIdentity.proportion)
//    assert(inGroupPayoffs.length == payoffs.length)
//
//    val strategyOut: Vector[Double] = getStrategy(salientIdentity, secondaryIdentity).out
//    // the strategy played by your salient identity against out-groups by that identity
//    // is the weighted elementwise sum of two vectors of the groups in your in-group
//    val outGroupStrategy = Utils.weightedSum(
//      strategyOut,
//      secondaryIdentity.proportion,
//      getStrategy(salientIdentity, secondaryIdentity.opposite).out,
//      secondaryIdentity.opposite.proportion)
//
//    // the strategy played against your salient identity by out-groups of that identity
//    // we have a vector of vectors of the proportion in each group of each strategy,
//    // take the row-wise sum
//    val oppositeOutGroupStrategy = Utils.weightedSum(
//      getStrategy(salientIdentity.opposite, secondaryIdentity).out,
//      secondaryIdentity.proportion,
//      getStrategy(salientIdentity.opposite, secondaryIdentity.opposite).out,
//      secondaryIdentity.opposite.proportion)
//
//    val outGroupPayoffs = payoffs.twoPopulationStrategyPayoffs(
//      outGroupStrategy, salientIdentity.proportion,
//      oppositeOutGroupStrategy, salientIdentity.opposite.proportion)
//
//    assert(outGroupPayoffs.length == payoffs.length)
//    /**
//      * Indexed by (in group strategy)(out group strategy) leads to the fitness for playing
//      * that in and out group strategy.
//      */
//    val jointFitness: Vector[Vector[Double]] = inGroupPayoffs.map(igp =>
//      outGroupPayoffs.map(igp * salientIdentity.proportion * secondaryIdentity.proportion +
//        _ * (1 - salientIdentity.proportion * secondaryIdentity.proportion)))
//
//    // compute the new proportions of each strategy
//    val newInGroupStrategy: Vector[Double] = strategyIn
//      .zip(jointFitness.map(_.sum)).map {
//      case (proportion, payoff) =>
//        proportion * (payoff / Utils.dotProduct(
//          strategyIn,
//          jointFitness.map(_.sum)))
//    }
//
//    val newOutGroupStrategy: Vector[Double] = strategyOut
//      .zip(jointFitness.transpose.map(_.sum)).map {
//      case (proportion, payoff) =>
//        proportion * (payoff / Utils.dotProduct(
//          strategyOut,
//          jointFitness.transpose.map(_.sum)))
//    }
//
//    assert(math.abs(newInGroupStrategy.sum - 1) < REPLICATION_EPSILON)
//    assert(math.abs(newOutGroupStrategy.sum - 1) < REPLICATION_EPSILON)
//
//    ArenaStrategy(newInGroupStrategy, newOutGroupStrategy)
//  }
//}