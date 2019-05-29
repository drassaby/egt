package intersectionaldisadvantage

import intersectionaldisadvantage.moderate.ModerateIntersectionalitySimulation


object Main {
  val RUNS = 500
  val MAX_GENERATIONS = 2000

  val simulation = ModerateIntersectionalitySimulation


  //  val strategies = 2 to 8 toVector
  val strategies = Vector(4, 6)
  val D = 0
  val P1_PROPORTION = .7
  val Q1_PROPORTION = .7


  //  val PAYOFFS = PayoffMatrix(strategies.map(s1 =>
  //    strategies.map(s2 => if (s1 + s2 <= 10) {
  //      (s1, s2)
  //    } else {
  //      (0, 0)
  //    })))
  val PAYOFFS: Map[(Arena, P), PayoffMatrix] = Map(
    (PArena, P1) -> PayoffMatrix(strategies.map(s1 =>
      strategies.map(s2 => if (s1 + s2 <= 10) {
        (s1, s2)
      } else {
        (D, 0)
      }))),
    (PArena, P2) -> PayoffMatrix(strategies.map(s1 =>
      strategies.map(s2 => if (s1 + s2 <= 10) {
        (s1, s2)
      } else {
        (0, D)
      }))),
    (QArena, P1) -> PayoffMatrix(strategies.map(s1 =>
      strategies.map(s2 => if (s1 + s2 <= 10) {
        (s1, s2)
      } else {
        (0, 0)
      }))),
    (QArena, P2) -> PayoffMatrix(strategies.map(s1 =>
      strategies.map(s2 => if (s1 + s2 <= 10) {
        (s1, s2)
      } else {
        (0, 0)
      }))),
  )


  def main(args: Array[String]): Unit = {
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

