package algorithms.greedy

import struct.Heap

/**
  * Created by yujieshui on 2017/7/13.
  */
object FightingPits {

  import Heap._

  type Strength = Int
  type Term = Int
  type Fighters = Heap[Int]

  trait Query

  case class Add(strength: Strength, term: Term) extends Query

  case class Attack(x: Term, y: Term) extends Query


  def attack(x: Heap[Strength], y: Heap[Strength]): Boolean = {
    if(x.isEmpty) false
    else if(y.isEmpty) true
    else {
      val strength = x.max
      !attack((1 to strength).foldLeft(y)((l, _) => dropMax(l)), x)
    }
  }

  def add(strength: Strength, fighters: Fighters): Heap[Term] =
    insert(strength, fighters)

  def solution(
                terms: Map[Term, Heap[Strength]],
                query: List[Query],
                acc: Seq[Int] = Nil
              ): Seq[Int] = {
    query match {
      case Nil                         => acc.reverse
      case Attack(x, y) :: tail        =>
        val win =
          if(attack(terms(x), terms(y))) x else y
        solution(terms, tail, win +: acc)
      case Add(strength, term) :: tail =>
        solution(
          terms.updated(term, add(strength, terms(term))),
          tail,
          acc
        )
    }
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: k :: q :: Nil = readListInt()
    val fighters = 1 to n map (_ => readListInt() match {
      case strength :: term :: Nil =>
        term -> strength
    }) groupBy (_._1) mapValues (_.map(_._2)) mapValues (e => Heap.apply(e))

    val query = (1 to q).toList map (_ => readListInt() match {
      case 1 :: p :: x :: Nil =>
        Add(p, x)
      case 2 :: x :: y :: Nil =>
        Attack(x, y)
    })

    val result = solution(fighters, query)
    println(result.mkString("\n"))
  }
}
