package intersectionaldisadvantage.moderate

import intersectionaldisadvantage._


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

          val strategyPayoffs: Vector[Double] = strategies.map(getPayoff(p, q, payoffs, _, newPop))
          val fitnesses = strategyPayoffs.map(_ / strategyPayoffs.sum)

          val newProportions = Utils.elementwiseProduct(strategyProportions, fitnesses)

          // find new proportions of each strategy (replicator dynamics)

          val newStrategyProportions = StrategyProportions(newProportions)
          // assign new proportions to be next population
          nextPop = nextPop + ((p, q) -> newStrategyProportions)
        }
      }

      // then record the strategy p1, q1 converges to in P and Q arena
      pqConvergence = pqConvergence :+ getConvergedStrategies(newPop)
    }

    pqConvergence
  }

  def getPayoff(p: P, q: Q, payoffs: Map[(Arena, P), PayoffMatrix],
                strategy: Strategy, newPop: Population): Double = {
    // P Arena

    // Q Arena
    

    // +
  }

  def getConvergedStrategies(newPop: Population, strategies: Vector[Strategy]): (Int, Int) = {
    val strategyProportions = newPop(P1, Q1).proportions

    var pConvergence = Map[Int, Double]()
    var qConvergence = Map[Int, Double]()

    strategyProportions.zip(strategies).foreach {
      case (prop, strat) =>
        pConvergence = pConvergence.updated(strat(1), pConvergence.getOrElse(strat(1), 0d) + prop)
        qConvergence = qConvergence.updated(strat(3), qConvergence.getOrElse(strat(3), 0d) + prop)
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



