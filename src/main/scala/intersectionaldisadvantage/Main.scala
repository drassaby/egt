package intersectionaldisadvantage

import intersectionaldisadvantage.minimal.MinimalIntersectionalitySimulation
import intersectionaldisadvantage.moderate.ModerateIntersectionalitySimulation


object Main {

  var P1_PROPORTION: Double = _
  var Q1_PROPORTION: Double = _
  var D: Double = _
  var strategies: Vector[Double] = _

  def main(args: Array[String]): Unit = {
    val MAX_GENERATIONS = 2000
    val simulation = if (args(0) == "minimal") {
      MinimalIntersectionalitySimulation
    } else {
      ModerateIntersectionalitySimulation
    }
    val RUNS: Int = args(1).toInt
    val P1_PROPORTION: Double = args(2).toDouble
    val Q1_PROPORTION: Double = args(3).toDouble
    val D: Double = args(4).toDouble
    val strategies: Vector[Double] = args.drop(5).map(_.toDouble).toVector

    this.strategies = strategies
    this.P1_PROPORTION = P1_PROPORTION
    this.Q1_PROPORTION = Q1_PROPORTION
    this.D = D


    val PAYOFFS: Map[(Arena, P), PayoffMatrix] = {
      /** Creates a payoff matrix, using the provided default if the demands are incompatible */
      def payoffs(disagreementPoint: (Double, Double)): PayoffMatrix = {
        PayoffMatrix(strategies.map(s1 =>
          strategies.map(s2 => if (s1 + s2 <= 10) {
            (s1, s2)
          } else {
            disagreementPoint
          })))
      }

      Map(
        (PArena, P1) -> payoffs(this.D, 0d),
        (PArena, P2) -> payoffs(0d, this.D),
        (QArena, P1) -> payoffs(0d, 0d),
        (QArena, P2) -> payoffs(0d, 0d),
      )
    }

    println(
      f"P1=$P1_PROPORTION, Q1=$Q1_PROPORTION, D=$D, " +
        f"strategies=$strategies, simulation=$simulation")
    val pqHighFrequencies = simulation(PAYOFFS, RUNS, MAX_GENERATIONS)

    val indexMap = pqHighFrequencies.groupBy(x => x)
      .mapValues(_.length.toDouble / pqHighFrequencies.length)

    val proportionsOfP1Q1Strategies =
      indexMap.foldLeft(Vector.fill(strategies.length)(Vector.fill(strategies.length)(0d))) {
        case (soFar, ((s1, s2), prop)) => soFar.updated(s1, soFar(s1).updated(s2, prop))
      }

    println(proportionsOfP1Q1Strategies.map(_.map("%.5f" format _).mkString(" ")).mkString("\n"))


  }

}

