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
      var oldPop: Population = fillStrategies(strategies, Vector.fill(_)(1d / strategies.length))
      var newPop: Population = fillStrategies(strategies, Utils.randFill)

      // while the population isn't stable
      var generation = 0
      while (!isStable(oldPop, newPop) && generation < maxGenerations) {
        var nextPop: Population = Map()
        // for each intersectional identity
        for ((p, q) <- newPop.keys) {
          // get payoffs for each strategy

          val strategyProportions = newPop(p, q).proportions

          val strategyPayoffs: Vector[Double] =
            strategies.map(getPayoff(p, q, payoffs, _, newPop, strategies))

          val fitnesses = strategyPayoffs.map(_ / (strategyPayoffs.sum))

//          println(s"fitnesses.sum = ${fitnesses.sum}")
          assert(math.abs(fitnesses.sum - 1) < REPLICATION_EPSILON)

          val newProportions = {
            val props = Utils.elementwiseProduct(strategyProportions, fitnesses)
            props.map(_ / props.sum)
          }

//          println(s"newProportions.sum = ${newProportions.sum}")
          assert(math.abs(newProportions.sum - 1) < REPLICATION_EPSILON)

          // find new proportions of each strategy (replicator dynamics)

          val newStrategyProportions = StrategyProportions(newProportions, strategies)
          // assign new proportions to be next population
          nextPop = nextPop + ((p, q) -> newStrategyProportions)
        }

        oldPop = newPop
        newPop = nextPop
        generation += 1
      }

      // then record the strategy p1, q1 converges to in P and Q arena
      pqConvergence = pqConvergence :+ getConvergedStrategies(newPop, strategies)
    }

    pqConvergence
  }

  /**
    *
    * @param payoffs  the payoff matrix for each arena for each P type
    * @param strategy the strategy to get the payoff for, when played by a (p, q) member
    * @param pop      the population: everyone else
    * @return
    */
  def getPayoff(p: P, q: Q, payoffs: Map[(Arena, P), PayoffMatrix],
                strategy: Strategy, pop: Population, strategies: Vector[Strategy]): Double = {

    val nStrats = payoffs.toVector.head._2.length
    
    // P Arena
    val pIn: Int = strategy.pIn
    val pOut: Int = strategy.pOut

    val pInGroupStrategies: Vector[Double] = Utils.weightedElementwiseSum(
      pop(p, q).strategyVector.pIn, q.proportion,
      pop(p, q.opposite).strategyVector.pIn, q.opposite.proportion)
    assert(pInGroupStrategies.length == nStrats)

    val pArenaInGroupPayoff: Double = 
      payoffs(PArena, p).averagePayoff(pIn, pInGroupStrategies) * p.proportion

    val pOutGroupStrategies: Vector[Double] = Utils.weightedElementwiseSum(
      pop(p.opposite, q).strategyVector.pOut, q.proportion,
      pop(p.opposite, q.opposite).strategyVector.pOut, q.opposite.proportion)
    assert(pOutGroupStrategies.length == nStrats)
    
    val pArenaOutGroupPayoff: Double =
      payoffs(PArena, p).averagePayoff(pOut, pOutGroupStrategies) * p.opposite.proportion
      
 
    // q Arena
    val qIn: Int = strategy.qIn
    val qOut: Int = strategy.qOut

    val qInGroupStrategies: Vector[Double] = Utils.weightedElementwiseSum(
      pop(p, q).strategyVector.qIn, p.proportion,
      pop((p.opposite, q)).strategyVector.qIn, p.opposite.proportion)
    assert(qInGroupStrategies.length == nStrats)

    val qArenaInGroupPayoff: Double =
      payoffs(QArena, p).averagePayoff(qIn, qInGroupStrategies) * q.proportion

    val qOutGroupStrategies: Vector[Double] = Utils.weightedElementwiseSum(
      pop(p, q.opposite).strategyVector.qOut, p.proportion,
      pop(p.opposite, q.opposite).strategyVector.qOut, p.opposite.proportion)
    assert(qOutGroupStrategies.length == nStrats)
    
    val qArenaOutGroupPayoff: Double =
      payoffs(QArena, p).averagePayoff(qOut, qOutGroupStrategies) * q.opposite.proportion
      


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

  private def fillStrategies(strategies: Vector[Strategy], strategyGenerator: Int => Vector[Double])
  : Map[(P, Q), StrategyProportions] = {
    Vector((P1, Q1), (P2, Q1), (P1, Q2), (P2, Q2))
      .map(_ -> StrategyProportions(strategyGenerator(strategies.length), strategies))
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



