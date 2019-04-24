object TwoAgentModel {

  def main(args: Array[String]): Unit = {
    val agent1 = ReinforcementLearner(Vector(1, 1))
    val agent2 = ReinforcementLearner(Vector(1, 1))
    val payoffs = PayoffMatrix(Vector("A", "B"), Vector(Vector((1, 1), (0, 0)), Vector((0, 0), (1, 1))))
    val simulation = TwoAgentSimulation(payoffs, agent1, agent2)
    val result = simulation.run(steps=100, repeats=10000)
    println(result)
  }
}

case class TwoAgentSimulation(payoffs: PayoffMatrix, agent1: ReinforcementLearner, agent2: ReinforcementLearner) {

  def run(steps: Int, repeats: Int): Map[(String, String), Int] = {
    Vector.fill(repeats)(runOneSimulation(steps))
    .groupBy(identity)
    .mapValues(_.length)
  }

  private def runOneSimulation(steps: Int): (String, String) = {
    val finalAgents = (1 to steps).fold((agent1, agent2))((agents, _) =>
    agents match {
      case (a1: ReinforcementLearner, a2: ReinforcementLearner) =>
        val s1 = a1.pickStrategy()
        val s2 = a2.pickStrategy()
        val (payoff1, payoff2) = payoffs(s1, s2)
        (a1.reinforce(s1, payoff1), a2.reinforce(s2, payoff2))
    })
    finalAgents match {
      case (a1: ReinforcementLearner, a2: ReinforcementLearner) =>
        (payoffs.strategyNames(a1.favoriteStrategy()),
        payoffs.strategyNames(a2.favoriteStrategy()))
    }
  }

}

/**
  *
  * @param strategyProbability Mapping from color to number of balls in urn
  */
case class ReinforcementLearner(strategyProbability: Vector[Int]) {

  def favoriteStrategy(): Int = strategyProbability.indexOf(strategyProbability.max)

  def reinforce(strategy: Int, payoff: Int): ReinforcementLearner = {
    this.copy(strategyProbability=
      strategyProbability.updated(strategy, strategyProbability(strategy) + payoff))
  }

  def pickStrategy(): Int = {
    var urnIndex = util.Random.nextInt(strategyProbability.sum)
    for (i <- strategyProbability.indices) {
      if (urnIndex < strategyProbability(i)) {
        return i
      }
      urnIndex -= strategyProbability(i)
    }
    throw new AssertionError("Couldn't pick a strategy")
  }
}

/**
  *
  * @param strategyNames Mapping from strategy to strategy name
  * @param payoffs Row index corresponds to the first players strategy, col index to the second's
  */
case class PayoffMatrix(strategyNames: Vector[String], payoffs: Vector[Vector[(Int, Int)]]) {

  def apply(strategy1: Int, strategy2: Int): (Int, Int) = payoffs(strategy1)(strategy2)
}
