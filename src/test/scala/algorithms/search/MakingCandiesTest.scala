package algorithms.search

import algorithms.search.MakingCandies._
import org.scalatest._

class MakingCandiesTest extends WordSpec with Matchers {

  "buy all" in {
    buyAll(Entity(3, 4, 5, 6)) shouldBe Entity(4, 4, 5, 1)
    buyAll(Entity(4, 4, 5, 1)) shouldBe Entity(4, 4, 5, 1)
    buyAll(Entity(4, 4, 5, 5)) shouldBe Entity(4, 5, 5, 0)
  }
}
