package intersectionaldisadvantage

object Utils {
  def weightedSum(v1: Vector[Double], p1: Double, v2: Vector[Double], p2: Double): Vector[Double] = {
    v1.map(_ * p1).zip(v2.map(_ * p2)).map { case (e1, e2) => e1 + e2 }
  }

  def dotProduct(v1: Vector[Double], v2: Vector[Double]): Double = {
    v1.zip(v2).map { case (e1, e2) => e1 * e2 }.sum
  }
}
