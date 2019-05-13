package intersectionaldisadvantage

import org.scalatest._

class PayoffsTest extends WordSpec with Matchers {

  "Payoff Matrix" should {
    "compute correctly in simple hawk-dove" in {
      val p = PayoffMatrix(Vector(
        Vector((1, 1), (1, 9)),
        Vector((9, 1), (0, 0)),
      ))

      val payoff = p.twoPopulationStrategyPayoffs(Vector(.5, .5), .5, Vector(.5,.5), .5)

      payoff(0) shouldBe > (.99)
      payoff(0) shouldBe < (1.01)

      payoff(1) shouldBe > (4.4)
      payoff(1) shouldBe < (4.6)
    }

    "work with different size groups" in {
      val p = PayoffMatrix(Vector(
        Vector((1, 1), (1, 9)),
        Vector((9, 1), (0, 0)),
      ))

      val payoff = p.twoPopulationStrategyPayoffs(Vector(.66, .34), .9, Vector(.34, .66), .1)

      payoff(0) shouldBe > (.99)
      payoff(0) shouldBe < (1.01)
      payoff(1) shouldBe (.34 * 9)
    }
  }

}
