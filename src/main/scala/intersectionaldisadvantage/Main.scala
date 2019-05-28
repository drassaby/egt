package intersectionaldisadvantage

import intersectionaldisadvantage.minimal.{MinimalIntersectionalitySimulation, PayoffMatrix}


object Main {
  val RUNS = 1000
  val MAX_GENERATIONS = 2000

//  val strategies = 2 to 8 toVector
  val strategies = Vector(4,6)
  val D = 3
  val P1_PROPORTION = .5
  val Q1_PROPORTION = .8


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
    val outcome = MinimalIntersectionalitySimulation(
      PAYOFFS,
      runs = RUNS,
      maxGenerations = MAX_GENERATIONS)

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

    val indexMap = pqHighFrequencies.groupBy(x => x)
      .mapValues(_.length.toDouble / pqHighFrequencies.length)

    val proportionsOfP1Q1Strategies =
      indexMap.foldLeft(Vector.fill(strategies.length)(Vector.fill(strategies.length)(0d))) {
      case (soFar, ((s1, s2), prop)) => soFar.updated(s1, soFar(s1).updated(s2, prop))
    }

    println(proportionsOfP1Q1Strategies.map(_.map("%.5f" format _).mkString(" ")).mkString("\n"))


  }

}

