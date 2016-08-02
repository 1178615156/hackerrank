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

  val grid_3 = lines2grid(Seq(
    "-d--d",
    "-d--d",
    "-----",
    "-----",
    "-----"
  ))
  val grid_4 = lines2grid(Seq(
    "-d--d-d--d",
    "-d--d-d--d",
    "---d---d--",
    "----d---d-",
    "--d-----b-"
  ))
  val grid_5 = lines2grid(Seq(
    "-d--d-d--d",
    "-d--d-d--d",
    "---d---d--",
    "----d---d-",
    "--d-----b-",
    "-d--d-d--d",
    "-d--d-d--d",
    "---d---d--",
    "----d---d-",
    "--d-----b-"
  ))
  val grid_6 = lines2grid(Seq(
    "-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d",
    "-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d",
    "---d---d-----d---d-----d---d-----d---d-----d---d--",
    "----d---d-----d---d-----d---d-----d---d-----d---d-",
    "--d-----b---d-----b---d-----b---d-----b---d-----b-",
    "-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d",
    "-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d",
    "---d---d-----d---d-----d---d-----d---d-----d---d--",
    "----d---d-----d---d-----d---d-----d---d-----d---d-",
    "--d-----b---d-----b---d-----b---d-----b---d-----b-",
    "-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d",
    "-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d",
    "---d---d-----d---d-----d---d-----d---d-----d---d--",
    "----d---d-----d---d-----d---d-----d---d-----d---d-",
    "--d-----b---d-----b---d-----b---d-----b---d-----b-",
    "-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d",
    "-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d",
    "---d---d-----d---d-----d---d-----d---d-----d---d--",
    "----d---d-----d---d-----d---d-----d---d-----d---d-",
    "--d-----b---d-----b---d-----b---d-----b---d-----b-",
    "-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d",
    "-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d",
    "---d---d-----d---d-----d---d-----d---d-----d---d--",
    "----d---d-----d---d-----d---d-----d---d-----d---d-",
    "--d-----b---d-----b---d-----b---d-----b---d-----b-",
    "-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d",
    "-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d",
    "---d---d-----d---d-----d---d-----d---d-----d---d--",
    "----d---d-----d---d-----d---d-----d---d-----d---d-",
    "--d-----b---d-----b---d-----b---d-----b---d-----b-",
    "-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d",
    "-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d",
    "---d---d-----d---d-----d---d-----d---d-----d---d--",
    "----d---d-----d---d-----d---d-----d---d-----d---d-",
    "--d-----b---d-----b---d-----b---d-----b---d-----b-",
    "-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d",
    "-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d",
    "---d---d-----d---d-----d---d-----d---d-----d---d--",
    "----d---d-----d---d-----d---d-----d---d-----d---d-",
    "--d-----b---d-----b---d-----b---d-----b---d-----b-",
    "-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d",
    "-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d",
    "---d---d-----d---d-----d---d-----d---d-----d---d--",
    "----d---d-----d---d-----d---d-----d---d-----d---d-",
    "--d-----b---d-----b---d-----b---d-----b---d-----b-",
    "-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d",
    "-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d-d--d",
    "---d---d-----d---d-----d---d-----d---d-----d---d--",
    "----d---d-----d---d-----d---d-----d---d-----d---d-",
    "--d-----b---d-----b---d-----b---d-----b---d-----b-"
  ))
  test("skip"){

  }
  test("is drity") {
    assert(asDirty(Point(0, 0), grid_1) === false)
    assert(asDirty(Point(1, 1), grid_1) === true)
    assert(asDirty(Point(1, 0), grid_2) === true)
  }
  test("next move point 1") {
    assert(nextMovePoint(Point(0, 0), grid_3) === Point(1, 0))
  }
  test("next move point") {
    assert(nextMovePoint(Point(1, 0), grid_3.updateCell(Cell(1, 0, '-'))) === Point(1, 1))
  }
  import org.scalameter._
  test("max") {
    val dirtys = grid_4.find(dirty).map(_.point)
    val robotPoint =Point(4, 9)
    val time = config().withWarmer(Warmer.Zero).measure{
      //nextMovePoint(Point(8, 9), grid_4)
      println( search(Seq(Work(Seq(),robotPoint,dirtys))).tail )
      println(skip(robotPoint,search(Seq(Work(Seq(),robotPoint,dirtys))).tail ))
   //List(Point(4,3), Point(3,2), Point(4,1), Point(4,0), Point(6,0), Point(6,1), Point(7,2), Point(8,3), Point(9,1), Point(9,0), Point(1,0), Point(1,1), Point(2,4))
      println(greedy(robotPoint,dirtys))
      println(skip(robotPoint,greedy(robotPoint,dirtys)))
//      assert(     search(Seq(Work(Seq(),robotPoint,dirtys))) === greedy(robotPoint,dirtys))
    }
    println(
      time
    )
  }

  test("grid_6"){
    val robotPoint = Point(23,42)
    val dirtys = grid_6.find(dirty).map(_.point)


    val time = config().withWarmer(Warmer.Zero).measure{
      //nextMovePoint(Point(8, 9), grid_4)
      val _search = search(Seq(Work(Seq(),robotPoint,dirtys))).tail
      println( _search )
      println(skip(robotPoint,_search ))
      //List(Point(4,3), Point(3,2), Point(4,1), Point(4,0), Point(6,0), Point(6,1), Point(7,2), Point(8,3), Point(9,1), Point(9,0), Point(1,0), Point(1,1), Point(2,4))
      println(greedy(robotPoint,dirtys))
      println(skip(robotPoint,greedy(robotPoint,dirtys)))
      //      assert(     search(Seq(Work(Seq(),robotPoint,dirtys))) === greedy(robotPoint,dirtys))
    }
    println(
      time
    )


  }
  test("xxxxxx"){
    var grid = grid_6

  }


}
