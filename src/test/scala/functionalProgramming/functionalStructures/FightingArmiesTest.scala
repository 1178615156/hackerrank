package functionalProgramming.functionalStructures

import java.io.File

import org.scalameter._
import org.scalatest.FunSuite
import scala.collection.immutable.{IntMap, TreeSet}
import scala.io.Source

/**
  * Created by yujieshui on 2016/8/2.
  */
class FightingArmiesTest extends FunSuite {

  import FightingArmies._
  import struct.Heap


  //  test("solution") {
  //    val out = solution(
  //      Map(1 -> mkArmy(Seq(3, 1, 10)), 2 -> mkArmy(Seq(3, 2, 20))),
  //      Seq(Merge(1, 2), FindStrongest(1), StrongestDied(1), FindStrongest(1))
  //    )
  //    assert(out === Seq(20, 10).map(_.toString))
  //  }

  def file(s: String) = this.getClass.getClassLoader.getResource(s).getFile

  test("a")  {
    val in =
      Source.fromFile(file("fp_ds_heap_10_in"))
        .getLines().toSeq
        .map(_.split(" ").toList.map(_.toInt)).iterator

    println(in.toList.groupBy(_.head).mapValues(_.length))

  }
  test("solution 10") {

    val in =
      Source.fromFile(file("fp_ds_heap_10_in"))
        .getLines().toSeq
        .map(_.split(" ").toList.map(_.toInt)).iterator

    val (n, evens) = read(() => in.next())
    val expected = Source.fromFile(file("fp_ds_heap_10_out"))
      .getLines().toSeq

    val startTime = System.currentTimeMillis()
    val out =
      solution(n, evens)
    val endTime = System.currentTimeMillis()


    assert(out.map(_.toString) === expected)
    println((endTime - startTime).toDouble / 1000)


  }


}
