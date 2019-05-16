object TwoPopRepDynMain {
  private val RUNS = 1000
  private val MAXGEN = 100 * 1000
  private val payoffsA =
    Vector(
      Vector(1.1, 3.1, 0.6, 3.6, 3.1),
      Vector(2.9, 0.9, 0.4, 3.4, 2.9),
      Vector(0.5, 3.5, 0.5, 3.5, 0.5),
      Vector(3.5, 0.5, 0.5, 3.5, 0.5))

  private val ATYPES = payoffsA.length
  private val BTYPES = payoffsA.head.length

  private val payoffsB =
    Vector(
      Vector(1.0, 3.0, 2.5, 1.5),
      Vector(3.0, 1.0, 1.5, 2.5),
      Vector(2.5, 2.5, 2.5, 2.5),
      Vector(1.5, 1.5, 1.5, 1.5),
      Vector(2.5, 2.5, 2.0, 2.0))
}
