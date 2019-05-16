package intersectionaldisadvantage


object Main {
  val RUNS = 10 * 1000
  val MAX_GENERATIONS = 2000

  val strategies = 4 to 6 toVector

  val PAYOFFS = PayoffMatrix(strategies.map(s1 =>
    strategies.map(s2 => if (s1 + s2 <= 10) {
      (s1, s2)
    } else {
      (0, 0)
    })))

  val P1_PROPORTION, Q1_PROPORTION = .9

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

        println(population)
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

