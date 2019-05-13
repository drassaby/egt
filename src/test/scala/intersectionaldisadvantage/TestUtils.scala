package intersectionaldisadvantage

import org.scalatest.{Matchers, WordSpec}

class TestUtils extends WordSpec with Matchers {
  "weightedSum" should {
    "work" in {
      val ws = Utils.weightedSum(Vector(.1, .2), 2, Vector(.15, .35), 1)

      ws shouldBe Vector(.35, .75)
    }
  }

  "dotProduct" should {
    "work" in {
      val dotted = Utils.dotProduct(Vector(1, 2, 3), Vector(4, 5, 6))

      dotted shouldBe 1 * 4 + 2 * 5 + 3 * 6
    }
  }

}
