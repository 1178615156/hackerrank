package functionalProgramming.functionalStructures


import java.io.{BufferedReader, InputStreamReader}

import scala.collection.BitSet
import scala.collection.immutable.IntMap
import struct.Heap

/**
  * Created by yujieshui on 2016/8/1.
  */

object FightingArmies {
  import Heap._


  type Soldier = Int
  type CombatAbility = Int
  type ArmyIndex = Int
  type Army = Heap[Int]
  type Armies = Map[Int, Army]


  trait Even

  case class FindStrongest(i: Int) extends Even

  case class StrongestDied(i: Int) extends Even

  case class Recruit(i: Int, c: CombatAbility) extends Even

  case class Merge(i: Int, j: Int) extends Even

  def findStrongest(army: Army): CombatAbility = army.max

  def strongestDied(army: Army): Army = {
    Heap.dropMax(army)
  }

  def recruit(soldier: Soldier, army: Army): Army = {
    Heap.insert(soldier, army)
  }

  def merge(a: Army, b: Army): Army ={
    Heap.merge(a, b)
  }

  def findArmyByIndex(int: Int, armies: Armies): Army =  {
    armies(int)
  }

  def updateArmy(i: Int, new_army: Army, armies: Armies): Armies =  {
    armies updated(i, new_army)
  }

  def dropArmy(i: Int, armies: Armies): Armies = {
    armies //- i
  }

  def solution(armies: Armies, even: Seq[Even]): Seq[String] = {

    val (final_armies, results) = even.foldLeft((armies, Seq[String]())) {

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
    results.reverse
  }

  def read(readListInt: () => List[Int]) = {

    val n :: q :: Nil = readListInt()
    val armies = IntMap(1 to n map (i => i -> Heap.empty[Int]): _*)
    val evens = 1 to q map (_ => readListInt()) map {
      case 1 :: i :: Nil      => FindStrongest(i)
      case 2 :: i :: Nil      => StrongestDied(i)
      case 3 :: i :: c :: Nil => Recruit(i, c)
      case 4 :: i :: j :: Nil => Merge(i, j)
    }
    (armies, evens)
  }

  def main(args: Array[String]): Unit = {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

    val (armies, evens) = read(() => readListInt())

    val out = solution(armies, evens)
    println(
      out.mkString("\n")
    )
  }
}
