package artificialIntelligence.botBuild

import org.scalatest.FunSuite

/**
  * Created by yuJieShui on 2016/7/28.
  */
class BotCleanLarge$Test extends FunSuite {

  import BotEntity._
  import BotCleanLarge._

  def lines2grid(lines: Seq[String]) = {
    val data =
      lines.indices zip lines map {
        case (height, line) => line.indices zip line map {
          case (weight, value) => Cell(weight, height, value)
        }
      }
    Grid(data: _*)
  }

  val grid_1 = lines2grid(Seq(
    "b---d",
    "-d--d",
    "--dd-",
    "--d--",
    "----d"
  ))
  val grid_2 = lines2grid(Seq(
    "-d--d",
    "-d--d",
    "--dd-",
    "--d--",
    "----d"
  ))


  test("is drity") {
    assert(asDirty(Point(0, 0), grid_1) === false)
    assert(asDirty(Point(1, 1), grid_1) === true)
    assert(asDirty(Point(1,0),grid_2) === true )
  }
}
