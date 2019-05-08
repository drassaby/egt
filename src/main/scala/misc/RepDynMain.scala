import scala.collection.mutable

object RepDynMain {
  private val RUNS = 10 // number of runs
  private val MAXGEN = 10000 // number of generations to run
  private val TYPES = 3 // number of unique strategies
  private val mu = 0.001 // proportion of mutation per generation
  private val EPSILON = 0.0001 // how close populations have to be for convergence

  private val pop = Vector.fill(TYPES)(1d / TYPES)
  private val oldPop = Vector.fill(TYPES)(0)

  private val stable = false
  private val gen = 0

  private val totals = Vector.fill(TYPES)(1d / TYPES)

  private val payoffs: Vector[Vector[Double]] = Vector(
    Vector(4, 1, 1),
    Vector(3, 0, 0),
    Vector(3, 0, 2),
  )

  def main(args: Array[String]): Unit = {
    println("Replicator Dynamics")

    var results = Vector[Vector[Double]]()

    for (run <- 1 to RUNS) {

      var oldPop = Vector.fill(TYPES)(0d)
      var newPop = randFill()

      var generation = 0
      while (!isStable(oldPop, newPop) && generation <= MAXGEN) {
        generation += 1
        oldPop = newPop
        newPop = replicate(payoffs, mu, oldPop)
      }
      results = results :+ newPop

      println(
        f"""
           |Generation: ${generation},
           |Results: ${newPop.map("%.3f" format _).mkString(",")}
           |""".stripMargin)
    }

    println(
      f"""
         |Results: ${results.map(_.map("%3f" format _).mkString(",")).mkString("\n")}
         |""".stripMargin)

  }


  def randFill(): Vector[Double] = {
    var x = 0
    var y = Vector.fill(TYPES)(0d)


    for (i <- 0 until TYPES) {
      //The negative logarithm is needed to ensure an unbaised distribution
      //Seems a bit strange if you don't know about random-point-picking
      //in Simplexes, but trust me...
      y = y.updated(i, 0 - math.log(util.Random.nextDouble()))
    }

    y.map(_ / y.sum)
  }

  /**
    * @return the new proportions of strategies in the population
    */
  def replicate(payoffs: Vector[Vector[Double]], mu: Double, pop: Vector[Double])
  : Vector[Double] = {
    val fitnesses = mutable.ArrayBuffer.fill(TYPES)(0d)

    for (i <- 0 until TYPES) {
      for (j <- 0 until TYPES) {
        fitnesses(i) += payoffs(i)(j) * pop(j)
      }
    }

    val next = pop.zip(fitnesses).map {
      case (frequency, fitness) =>
        frequency * ((1 - mu) * fitness / fitnesses.sum) + (mu / TYPES)
    }
    next.map(_ / next.sum)
  }

  def isStable(oldPop: Vector[Double], newPop: Vector[Double]): Boolean = {
    oldPop.corresponds(newPop)((o, n) => math.abs(o - n) < EPSILON)
  }
}
