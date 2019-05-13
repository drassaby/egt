package intersectionaldisadvantage


object Main {
  val RUNS = 100
  val MAX_GENERATIONS = 1000

  val PAYOFFS = PayoffMatrix(Vector(
    Vector((4, 4), (4, 6)),
    Vector((6, 4), (0, 0)),
  ))

  val P1_PROPORTION, Q1_PROPORTION = .9

  def main(args: Array[String]): Unit = {
    val outcome = MinimalIntersectionalitySimulation(
      PAYOFFS,
      runs = RUNS,
      maxGenerations = MAX_GENERATIONS)

    var pqHighFrequencies = Vector[(P, Q)]()

//    println(outcome)
    // Record the results from this run
    outcome.foreach {
      population => {
        println(population)
        val pHigh = if (population(P1, Q1).p.out.indexOf(population(P1, Q1).p.out.max) == 1) {
          P1
        } else {
          P2
        }

        val qHigh = if (population(P1, Q1).q.out.indexOf(population(P1, Q1).q.out.max) == 1) {
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

