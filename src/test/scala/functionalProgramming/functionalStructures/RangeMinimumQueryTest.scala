package functionalProgramming.functionalStructures

import java.io.File

import org.scalatest.FunSuite

import scala.io.Source

/**
  * Created by yujieshui on 2016/8/19.
  */
class RangeMinimumQueryTest extends FunSuite {

  import functionalProgramming.BinaryTree._
  import RangeMinimumQuery._

  val array = Seq(1, 2, 3, 4, 5)
  val tree  = apply(array, 0, array.size)

  test("right tree") {

    0 to array.size map { i =>
      rightTree(i)(tree).flatMap(tree2value).toSet === array.drop(i).toSet
    }
  }

  test("left tree") {
    Seq(5, 4, 3, 2, 1, 0) map { i =>
      rightTree(i)(tree).flatMap(tree2value).toSet === array.dropRight(5 - i).toSet
    }
  }

  test("cut out") {
    val trees = rightTree(0)(tree) ++ leftTree(2)(tree)
//    cutoutTree(0, 2)(trees).flatMap(tree2value).toSet === (Set(0, 1))
  }

  test("solution") {
    val array = ("10 20 30 40 11 22 33 44 15 5".split(" ").map(_.toInt).toSeq)
    val tree = apply(array, 0, array.size)
    assert(solution(0, 5 + 1)(tree) === 10)
    assert(solution(1, 2 + 1)(tree) === 20)
    assert(solution(8, 9 + 1)(tree) === 5)
    assert(solution(0, 9 + 1)(tree) === 5)
    assert(solution(4, 6 + 1)(tree) === 11)
  }

  def rootFile = {
    println(System.getProperty("user.dir"))
    System.getProperty("user.dir") + "\\target\\scala-2.11\\test-classes"
  }

  test("solution case 3") {
    val fileLines = Source.fromFile(new File(s"$rootFile\\fp-rmq-c5")).getLines().toSeq.tail

    val array = fileLines.head.split(" ").map(_.toInt).toSeq
    val tree = apply(array, 0, array.size)

    assert(solution(640,731 + 1)(tree) === -975)
    val in: Seq[(Int, Int)] = fileLines.tail.map(_.split(" ")).map(l => l(0).toInt -> l(1).toInt)


    val expected = Source.fromFile(s"$rootFile\\fp-rmq-c5-expected").getLines().toSeq.map(_.toInt)
    val result = in zip expected foreach {
      case ((start: Int, end), e) =>
        assert(
          solution(start, end + 1)(tree) === e
//          ,
//          (start, end,array(start),array((end+1)))
        )
    }


  }
}
