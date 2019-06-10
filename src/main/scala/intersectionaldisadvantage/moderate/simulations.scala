package intersectionaldisadvantage.moderate

import intersectionaldisadvantage._


object ModerateIntersectionalitySimulation extends TwoArenaSimulation {
  override def toString(): String = "Moderate"

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


    // for each run
    var pqConvergence: Vector[(Int,Int)] = (1 to runs).toVector.par.map(run => {
      if (run % 100 == 0) {
        println(s"Ran ${run} out of ${runs} runs")
      }
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

          val strategyPayoffs: IndexedSeq[Double] =
            strategies.map(getPayoff(p, q, payoffs, _, newPop, strategies))

          val fitnesses = strategyPayoffs.map(_ / strategyPayoffs.sum)

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
      val out = getConvergedStrategies(newPop, strategies)
      println(out)
      out
    }).toVector

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
                strategy: Strategy, pop: Population, strategies: IndexedSeq[Strategy]): Double = {

    val nStrats = payoffs.toVector.head._2.length
    
    // P Arena
    val pIn: Int = strategy.pIn
    val pOut: Int = strategy.pOut

    val pInGroupStrategies: IndexedSeq[Double] = Utils.weightedElementwiseSum(
      pop(p, q).strategyVector.pIn, q.proportion,
      pop(p, q.opposite).strategyVector.pIn, q.opposite.proportion)
    assert(pInGroupStrategies.length == nStrats)

    val pArenaInGroupPayoff: Double = 
      payoffs(PArena, p).averagePayoff(pIn, pInGroupStrategies) * p.proportion

    val pOutGroupStrategies: IndexedSeq[Double] = Utils.weightedElementwiseSum(
      pop(p.opposite, q).strategyVector.pOut, q.proportion,
      pop(p.opposite, q.opposite).strategyVector.pOut, q.opposite.proportion)
    assert(pOutGroupStrategies.length == nStrats)
    
    val pArenaOutGroupPayoff: Double =
      payoffs(PArena, p).averagePayoff(pOut, pOutGroupStrategies) * p.opposite.proportion
      
 
    // q Arena
    val qIn: Int = strategy.qIn
    val qOut: Int = strategy.qOut

    val qInGroupStrategies: IndexedSeq[Double] = Utils.weightedElementwiseSum(
      pop(p, q).strategyVector.qIn, p.proportion,
      pop((p.opposite, q)).strategyVector.qIn, p.opposite.proportion)
    assert(qInGroupStrategies.length == nStrats)

    val qArenaInGroupPayoff: Double =
      payoffs(QArena, p).averagePayoff(qIn, qInGroupStrategies) * q.proportion

    val qOutGroupStrategies: IndexedSeq[Double] = Utils.weightedElementwiseSum(
      pop(p, q.opposite).strategyVector.qOut, p.proportion,
      pop(p.opposite, q.opposite).strategyVector.qOut, p.opposite.proportion)
    assert(qOutGroupStrategies.length == nStrats)
    
    val qArenaOutGroupPayoff: Double =
      payoffs(QArena, p).averagePayoff(qOut, qOutGroupStrategies) * q.opposite.proportion
      

    pArenaInGroupPayoff + pArenaOutGroupPayoff + qArenaInGroupPayoff + qArenaOutGroupPayoff
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

  private def fillStrategies(strategies: IndexedSeq[Strategy], strategyGenerator: Int => IndexedSeq[Double])
  : Map[(P, Q), StrategyProportions] = {
    Vector((P1, Q1), (P2, Q1), (P1, Q2), (P2, Q2))
      .map(_ -> StrategyProportions(strategyGenerator(strategies.length), strategies))
      .toMap
  }


  private def isStable(oldPop: Population, newPop: Population): Boolean = {
    def vectorStable(v1: IndexedSeq[Double], v2: IndexedSeq[Double]): Boolean = {
      v1.corresponds(v2)((e1, e2) => math.abs(e1 - e2) < STABILITY_EPSILON)
    }

    oldPop.keys.forall(k =>
      vectorStable(oldPop(k).proportions, newPop(k).proportions)
    )
  }
}



