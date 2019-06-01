package intersectionaldisadvantage

import intersectionaldisadvantage.moderate.ModerateIntersectionalitySimulation


object Main {

  var P1_PROPORTION: Double = _
  var Q1_PROPORTION: Double = _
  var D: Double = _
  var strategies: Vector[Double] = _

  def main(args: Array[String]): Unit = {
    val MAX_GENERATIONS = 2000
    val simulation = ModerateIntersectionalitySimulation

    val RUNS: Int = args(0).toInt
    val P1_PROPORTION: Double = args(1).toDouble
    val Q1_PROPORTION: Double = args(2).toDouble
    val D: Double = args(3).toDouble
    val strategies: Vector[Double] = args.drop(4).map(_.toDouble).toVector

    this.strategies = strategies
    this.P1_PROPORTION = P1_PROPORTION
    this.Q1_PROPORTION = Q1_PROPORTION
    this.D = D


    val PAYOFFS: Map[(Arena, P), PayoffMatrix] = Map(
      (PArena, P1) -> PayoffMatrix(strategies.map(s1 =>
        strategies.map(s2 => if (s1 + s2 <= 10) {
          (s1, s2)
        } else {
          (D, 0d)
        }))),
      (PArena, P2) -> PayoffMatrix(strategies.map(s1 =>
        strategies.map(s2 => if (s1 + s2 <= 10) {
          (s1, s2)
        } else {
          (0d, D)
        }))),
      (QArena, P1) -> PayoffMatrix(strategies.map(s1 =>
        strategies.map(s2 => if (s1 + s2 <= 10) {
          (s1, s2)
        } else {
          (0d, 0d)
        }))),
      (QArena, P2) -> PayoffMatrix(strategies.map(s1 =>
        strategies.map(s2 => if (s1 + s2 <= 10) {
          (s1, s2)
        } else {
          (0d, 0d)
        }))),
    )

    println(
      f"P1=$P1_PROPORTION, Q1=$Q1_PROPORTION, D=$D, " +
        f"strategies=$strategies, simulation=$simulation")
    val pqHighFrequencies = simulation(
      PAYOFFS,
      runs = RUNS,
      maxGenerations = MAX_GENERATIONS)

    val indexMap = pqHighFrequencies.groupBy(x => x)
      .mapValues(_.length.toDouble / pqHighFrequencies.length)

    val proportionsOfP1Q1Strategies =
      indexMap.foldLeft(Vector.fill(strategies.length)(Vector.fill(strategies.length)(0d))) {
        case (soFar, ((s1, s2), prop)) => soFar.updated(s1, soFar(s1).updated(s2, prop))
      }

    println(proportionsOfP1Q1Strategies.map(_.map("%.5f" format _).mkString(" ")).mkString("\n"))


  }

}

