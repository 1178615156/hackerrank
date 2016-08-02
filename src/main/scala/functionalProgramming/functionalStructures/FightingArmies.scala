package functionalProgramming.functionalStructures


import java.io.{BufferedReader, InputStreamReader}

import scala.collection.BitSet
import scala.collection.immutable.IntMap
import struct.Heap

/**
  * Created by yujieshui on 2016/8/1.
  */
object FightingArmies {

  case class TimeRecd(var time: Long = 0) {
    override def toString: String = time.toString

    def apply() = time
  }

  def time[T](timeRecd: TimeRecd)(_rt: => T) = {
    val st = System.nanoTime()
    val rt = _rt
    val et = System.nanoTime()
    timeRecd.time += et - st
    rt
  }

  type Soldier = Int
  type CombatAbility = Int
  type ArmyIndex = Int
  type Army = Heap[Int]
  type Armies = Map[Int, Army]

  var recruitTime    = TimeRecd()
  var diedTime       = TimeRecd()
  var updateArmyTime = TimeRecd()
  var mergeTime      = TimeRecd()
  var findArmyTime   = TimeRecd()
  var dropTime       = TimeRecd()
  var reverseTime    = TimeRecd()

  trait Even

  case class FindStrongest(i: Int) extends Even

  case class StrongestDied(i: Int) extends Even

  case class Recruit(i: Int, c: CombatAbility) extends Even

  case class Merge(i: Int, j: Int) extends Even

  def findStrongest(army: Army): CombatAbility = army.max

  def strongestDied(army: Army): Army = time(diedTime) {
    Heap.dropMax(army)
  }

  def recruit(soldier: Soldier, army: Army): Army = time(recruitTime) {
    Heap.insert(soldier, army)
  }

  def merge(a: Army, b: Army): Army = time(mergeTime) {
    Heap.merge(a, b)
  }

  def findArmyByIndex(int: Int, armies: Armies): Army = time(findArmyTime) {
    armies(int)
  }

  def updateArmy(i: Int, new_army: Army, armies: Armies): Armies = time(updateArmyTime) {
    armies updated(i, new_army)
  }

  def dropArmy(i: Int, armies: Armies): Armies = time(dropTime) {
    armies // - i
  }

  val zeroArmy = IntMap.empty[Army]

  def solution(armies: Armies, even: Seq[Even]): Seq[String] = {

    val (final_armies, result) = even.foldLeft((armies, Seq[String]())) {

      case ((acc_armies, result), FindStrongest(i)) =>

        val resultHead = findStrongest(findArmyByIndex(i, acc_armies)).toString
        acc_armies -> (resultHead +: result)

      case ((acc_armies, result), StrongestDied(i)) =>
        val new_army = strongestDied(findArmyByIndex(i, acc_armies))
        updateArmy(i, new_army, acc_armies) -> result

      case ((acc_armies, result), Recruit(i, c)) =>
        val new_army = recruit(c, findArmyByIndex(i, acc_armies))
        updateArmy(i, new_army, acc_armies) -> result

      case ((acc_armies, result), Merge(i, j)) =>
        val new_army = merge(findArmyByIndex(i, acc_armies), findArmyByIndex(j, acc_armies))

        updateArmy(i, new_army, dropArmy(j, acc_armies)) -> result
    }
    result.reverse
  }

  def read(readListInt: () => List[Int]) = {

    val n :: q :: Nil = readListInt()
    val armies = IntMap((1 to n map (i => i -> Heap.empty[Int])): _*)
    val evens = 1 to q map (_ => readListInt()) map {
      case 1 :: i :: Nil      => FindStrongest(i)
      case 2 :: i :: Nil      => StrongestDied(i)
      case 3 :: i :: c :: Nil => Recruit(i, c)
      case 4 :: i :: j :: Nil => Merge(i, j)
    }
    (armies, evens)
  }

  def main(args: Array[String]): Unit = {
    val bi = new BufferedReader(new InputStreamReader(System.in))
    def readListInt() = bi.readLine().split(" ").toList.map(_.toInt)

    val (armies, evens) = read(() => readListInt())

    val out = solution(armies, evens)
    println(
      out.mkString("\n")
    )
  }
}
