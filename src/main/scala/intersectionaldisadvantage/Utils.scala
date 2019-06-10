package intersectionaldisadvantage

object Utils {
  val RANDOM = new scala.util.Random(new java.security.SecureRandom())

  def weightedElementwiseSum(v1: IndexedSeq[Double], p1: Double, v2: IndexedSeq[Double], p2: Double): IndexedSeq[Double] = {
    v1.map(_ * p1).zip(v2.map(_ * p2)).map { case (e1, e2) => e1 + e2 }
  }

  def dotProduct(v1: IndexedSeq[Double], v2: IndexedSeq[Double]): Double = {
    elementwiseProduct(v1, v2).sum
  }

  def elementwiseProduct(v1: IndexedSeq[Double], v2: IndexedSeq[Double]): IndexedSeq[Double] = {
    v1.zip(v2).map { case (e1, e2) => e1 * e2 }
  }



  /**
    * @param n the length of the IndexedSeq
    * @return a IndexedSeq with samples from a uniform distribution that sum to one.
    */
  def randFill(n: Int): Vector[Double] = {
    // The negative logarithm is needed to ensure an unbiased distribution
    // Seems a bit strange if you don't know about random-point-picking
    // in Simplexes, but trust me...
    val y = Vector.fill(n)(-math.log(RANDOM.nextDouble()))
    y.map(_ / y.sum)
  }
}
