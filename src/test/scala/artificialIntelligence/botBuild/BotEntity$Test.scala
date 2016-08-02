package artificialIntelligence.botBuild

import org.scalatest.FunSuite

/**
  * Created by yuJieShui on 2016/7/28.
  */
class BotEntity$Test extends FunSuite {

  import BotEntity._

  test("move step") {
    moveStep(Point(0, 0), Point(0, 0)).isEmpty
    moveStep(Point(0, 0), Point(1, 1)) === Move.RIGHT :: Move.DOWN :: Nil
  }
}
