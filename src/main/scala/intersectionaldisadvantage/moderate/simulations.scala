package intersectionaldisadvantage.moderate

import intersectionaldisadvantage._

import scala.collection.mutable


object ModerateIntersectionalitySimulation extends TwoArenaSimulation {

  override def apply(payoffs: Map[(Arena, P), PayoffMatrix], runs: Int, maxGenerations: Int)
  : Vector[(Int, Int)] = {

    val numBargianingNorms = payoffs.toVector.head._2.length
    val strategies: Vector[Strategy] =
      Vector.tabulate(
        numBargianingNorms,
        numBargianingNorms,
        numBargianingNorms,
        numBargianingNorms)(
        (pin, pout, qin, qout) => Strategy(pin, pout, qin, qout)
      ).flatten.flatten.flatten

    val numStrategies = strategies.length


    var pqConvergence = Vector[(Int, Int)]()
    // for each run
    for (run <- 0 to runs) {
      var oldPop: Population = fillStrategies(numStrategies, Vector.fill(_)(0d))
      var newPop: Population = fillStrategies(numStrategies, Utils.randFill)

      // while the population isn't stable
      while (!isStable(oldPop, newPop)) {
        var nextPop: Population = Map()
        // for each intersectional identity
        for ((p, q) <- newPop.keys) {
          // get payoffs for each strategy

          val strategyProportions = newPop(p, q).proportions

          val strategyPayoffs: Vector[Double] =
            strategies.map(getPayoff(p, q, payoffs, _, newPop, strategies))

          val fitnesses = strategyPayoffs.map(_ / strategyPayoffs.sum)

          val newProportions = Utils.elementwiseProduct(strategyProportions, fitnesses)

          // find new proportions of each strategy (replicator dynamics)

          val newStrategyProportions = StrategyProportions(newProportions)
          // assign new proportions to be next population
          nextPop = nextPop + ((p, q) -> newStrategyProportions)
        }
      }

      // then record the strategy p1, q1 converges to in P and Q arena
      pqConvergence = pqConvergence :+ getConvergedStrategies(newPop, strategies)
    }

    pqConvergence
  }

  /**
    *
    * @param p
    * @param q
    * @param payoffs  the payoff matrix for each arena for each P type
    * @param strategy the strategy to get the payoff for, when played by a (p, q) member
    * @param pop      the population: everyone else
    * @return
    */
  def getPayoff(p: P, q: Q, payoffs: Map[(Arena, P), PayoffMatrix],
                strategy: Strategy, pop: Population, strategies: Vector[Strategy]): Double = {


    // P Arena
    val pIn: Int = strategy.pIn
    val pOut: Int = strategy.pOut
    val ourPStrategy = Vector.tabulate(strategies.length)(x => if (x == pIn) 1 else 0)
    val theirPStrategy: Vector[Double] = pop.foldLeft(Vector(0d, 0d)) {
      case (soFar, ((stratP, stratQ), stratProps)) =>
        if (stratP == p) {
          soFar
        } else {
          val theirStrat = strategyFrequency(stratProps, strategies)(1)
          Utils.weightedElementwiseSum(
            soFar, 1, theirStrat, stratQ.proportion)
        }
    }

    val pArenaInGroupPayoff: Double = payoffs(PArena, p)
      .twoPopulationStrategyPayoffs(ourPStrategy, p.proportion).sum
    val pArenaOutGroupPayoff: Double = payoffs(PArena, p).twoPopulationStrategyPayoffs(
      ourPStrategy, p.proportion,
      theirPStrategy, p.opposite.proportion).sum


    pArenaInGroupPayoff + pArenaOutGroupPayoff + qArenaInGroupPayoff + qArenaOutGroupPayoff
  }

  /**
    *
    * @param strategyProportions proportions that each strategy e.x. <1, 1, 0, 1> is played
    * @return proportions of a strategy played in each arena, e.x. <<.4, .6>, <.1, .9>, …>
    */
  def strategyFrequency(strategyProportions: StrategyProportions, strategies: Vector[Strategy])
  : Vector[Vector[Double]] = {
    var frequencies = mutable.ArrayBuffer.fill(4)(mutable.ArrayBuffer.fill(strategies.length)(0d))

    strategyProportions.proportions.zip(strategies).foreach {
      case (prop, strat) =>
        frequencies(0)(strat.pIn) += prop
        frequencies(1)(strat.pOut) += prop
        frequencies(2)(strat.qIn) += prop
        frequencies(3)(strat.qOut) += prop
    }

    frequencies.map(_.toVector).toVector
  }

  def getConvergedStrategies(newPop: Population, strategies: Vector[Strategy]): (Int, Int) = {
    val strategyProportions = newPop(P1, Q1).proportions

    var pConvergence = Map[Int, Double]()
    var qConvergence = Map[Int, Double]()

    strategyProportions.zip(strategies).foreach {
      case (prop, strat) =>
        pConvergence = pConvergence.updated(strat.pOut, pConvergence.getOrElse(strat.pOut, 0d) + prop)
        qConvergence = qConvergence.updated(strat.qOut, qConvergence.getOrElse(strat.qOut, 0d) + prop)
    }

    (pConvergence.toVector.maxBy(_._2)._1,
      qConvergence.toVector.maxBy(_._2)._1)
  }

  private def fillStrategies(numStrategies: Int, thunk: Int => Vector[Double]): Map[(P, Q), StrategyProportions] = {
    Vector((P1, Q1), (P2, Q1), (P1, Q2), (P2, Q2))
      .map(_ -> StrategyProportions(
        thunk(numStrategies)))
      .toMap
  }


  private def isStable(oldPop: Population, newPop: Population): Boolean = {
    def vectorStable(v1: Vector[Double], v2: Vector[Double]): Boolean = {
      v1.corresponds(v2)((e1, e2) => math.abs(e1 - e2) < STABILITY_EPSILON)
    }

    oldPop.keys.forall(k =>
      vectorStable(oldPop(k).proportions, newPop(k).proportions)
    )
  }
}



