package intersectionaldisadvantage.minimal

case class PayoffMatrix(payoffs: Vector[Vector[(Int, Int)]]) {
  // payoffs must be square and nonempty
  assert(payoffs.length == payoffs.head.length)

  def length: Int = payoffs.length


  def strategyPayoffs(player1: Vector[Double], p1prop: Double): Vector[Double] = {
    twoPopulationStrategyPayoffs(player1, p1prop, player1, p1prop)
  }

  /**
    *
    * @param player1 how often player 1 plays each strategy
    * @param p1prop what proportion of the population is made of player1
    * @param player2 how often player 2 plays each strategy
    * @param p2prop what proportion of the population is made of player2
    * @return the average payoff for each strategy when played by player1 against player2
    */
  def twoPopulationStrategyPayoffs(player1: Vector[Double], p1prop: Double,
                                   player2: Vector[Double], p2prop: Double
                                  ): Vector[Double] = {

    assert(math.abs(player1.sum - 1) < .001)
    assert(math.abs(player2.sum - 1) < .001)

    (for (i <- player1.indices) yield {
      var strategyPayoff = 0d

      for (j <- player2.indices) {
        strategyPayoff +=
          // the proportion of the time that I play i against you playing j
          // multiplied by the payoff for i against j
          // multiplied by the proportion of the time we face off.
          (player2(j) * payoffs(i)(j)._1 * p1prop
            + player2(j) * payoffs(i)(j)._1 * p2prop)
      }
      strategyPayoff
    }).toVector
  }
}
