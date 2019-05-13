package intersectionaldisadvantage


object Main {
  val RUNS = 200
  val MAX_GENERATIONS = 2000

  val PAYOFFS = PayoffMatrix(Vector(
    Vector((4, 4), (4, 6)),
    Vector((6, 4), (0, 0)),
  ))

  val P1_PROPORTION, Q1_PROPORTION = .9

  def main(args: Array[String]): Unit = {
    val outcome = ModerateIntersectionalitySimulation(
      PAYOFFS,
      runs = RUNS,
      maxGenerations = MAX_GENERATIONS)

    var pqHighFrequencies = Vector[(P, Q)]()

//    println(outcome)
    // Record the results from this run
    outcome.foreach {
      population => {
        def maxIndex(vector: Vector[Double]) = {
          vector.indexOf(vector.max)
        }
        println(population)
        val pHigh = if (maxIndex(population(P1, Q1).p.out) == PAYOFFS.length - 1) {
          P1
        } else {
          P2
        }

        val qHigh = if (maxIndex(population(P1, Q1).q.out) == PAYOFFS.length - 1) {
          Q1
        } else {
          Q2
        }
        pqHighFrequencies = pqHighFrequencies :+ (pHigh, qHigh)

      }
    }
    println(pqHighFrequencies.groupBy(x => x)
      .mapValues(_.length.toDouble / pqHighFrequencies.length))

  }

}

