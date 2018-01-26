package functionalProgramming.functionalStructures


/**
  * Created by yujieshui on 2016/8/1.
  */


import scala.util.matching.Regex

import struct.Heap

object FightingArmies {

  import scala.collection.immutable.{IntMap, TreeMap, TreeSet}
  import scala.annotation.tailrec
  import Heap._


  type Soldier = Int
  type CombatAbility = Int
  type ArmyIndex = Int
  type Army = Heap[Int]
  type Armies = Array[Army] // Map[Int, Army]


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
    if(army == null)
      single(soldier)
    else
      Heap.insert(soldier, army)
  }

  def merge(a: Army, b: Army): Army = {
    Heap.merge(a, b)
  }

  def findArmyByIndex(int: Int, armies: Armies): Army = {
    armies(int)
  }

  def updateArmy(i: Int, new_army: Army, armies: Armies): Armies = {
    armies.update(i, new_army)
    armies
  }

  def dropArmy(i: Int, armies: Armies): Armies = {
    armies //- i
  }

  def mkArmy(seq: Seq[Soldier]) = Heap.apply(seq)

  def solution(n: Int, even: Seq[Even]): Seq[Int] = {

    @tailrec
    def execCmd(cmds: Seq[Even], armies: Armies, result: Seq[Int]): Seq[Int] = {
      if(cmds.isEmpty) result.reverse
      else {
        cmds.head match {

          case FindStrongest(i) =>
            execCmd(cmds.tail, armies, findStrongest(findArmyByIndex(i, armies)) +: result)

          case StrongestDied(i) =>
            val new_army = strongestDied(findArmyByIndex(i, armies))
            val new_armies = updateArmy(i, new_army, armies)
            execCmd(cmds.tail, new_armies, result)

          case Recruit(i, c) =>
            val new_army = recruit(c, findArmyByIndex(i, armies))
            val new_armies = updateArmy(i,new_army,armies)
            execCmd(cmds.tail, new_armies, result)

          case Merge(i,j)=>
            val new_army = merge(findArmyByIndex(i, armies), findArmyByIndex(j, armies))
            val new_armies = updateArmy(i,new_army,armies)
            execCmd(cmds.tail, new_armies, result)

        }
      }
    }
    val armies: Armies = new Array(n + 1)
    val (headRecruit, other) = even.span(_.isInstanceOf[Recruit])
    headRecruit.foreach(x =>{
      val e = x.asInstanceOf[Recruit]
      armies.update(e.i, Heap.single(e.c))
    })

    execCmd(other,armies,Nil)

  }

  def read(readListInt: () => List[Int]) = {

    val n :: q :: Nil = readListInt()
    val evens = 1 to q map (_ => readListInt()) map {
      case 1 :: i :: Nil      => FindStrongest(i)
      case 2 :: i :: Nil      => StrongestDied(i)
      case 3 :: i :: c :: Nil => Recruit(i, c)
      case 4 :: i :: j :: Nil => Merge(i, j)
    }
    n -> evens
  }

  def main(args: Array[String]): Unit = {

    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

    val n :: q :: Nil = readListInt()
    val evens = 1 to q map (_ => readListInt() match {
      case 1 :: i :: Nil      => FindStrongest(i)
      case 2 :: i :: Nil      => StrongestDied(i)
      case 3 :: i :: c :: Nil => Recruit(i, c)
      case 4 :: i :: j :: Nil => Merge(i, j)
    })
    val out = solution(n, evens)
    println(
      out.mkString("\n")
    )
  }
}
