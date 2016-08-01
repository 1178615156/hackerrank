package functionalProgramming.functionalStructures

import scala.collection.BitSet
import scala.collection.immutable.IntMap

/**
  * Created by yujieshui on 2016/8/1.
  */
object FightingArmies {
  type Soldier = Int
  type CombatAbility = Int
  type ArmyIndex = Int
  type Army = BitSet
  type Armies = Map[Int, Army]

  case class Army(set: BitSet,map:IntMap[Int]){
    def max = set.max
    def - ()
  }
  //  val x: BitSet =BitSet(1,2,3)
  trait Even

  case class FindStrongest(i: Int) extends Even

  case class StrongestDied(i: Int) extends Even

  case class Recruit(i: Int, c: CombatAbility) extends Even

  case class Merge(i: Int, j: Int) extends Even

  def findStrongest(army: Army): CombatAbility = army.max

  def strongestDied(army: Army): Army = army - army.max

  def recruit(soldier: Soldier, army: Army): Army = army + soldier

  def merge(a: Army, b: Army): Army = ???


  def findArmyByIndex(int: Int, armies: Armies): Army = armies(int)

  def updateArmy(i: Int, new_army: Army, armies: Armies): Armies = armies + (i -> new_army)

  def dropArmy(i: Int, armies: Armies): Armies = armies - i

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

      case ((acc_armies, result), Merge(i, j))   =>
        val new_army = merge(findArmyByIndex(i, acc_armies), findArmyByIndex(j, acc_armies))

        updateArmy(i, new_army, dropArmy(j, acc_armies)) -> result
    }
    result
  }

  def main(args: Array[String]): Unit = {

  }
}
