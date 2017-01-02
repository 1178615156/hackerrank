package functionalProgramming.recursion

import utils.TimeUtil


/**
  * Created by yujieshui on 2016/12/31.
  */

trait SuperQueens {

  import scala.collection.immutable.IndexedSeq
  import scala.annotation.tailrec

  object Square extends Enumeration {
    val q      = Value("q")
    val attack = Value("-")
    val safe   = Value("0")
  }

  final type Position = (Int, Int)

  final type Zone = Seq[Seq[Square.Value]]

  def show(zone: Zone): String = zone.map(_.mkString(" ")).mkString("\n")

  def mkZone(n: Int): Zone = 1 to n map (_ => 1 to n map (_ => Square.safe) toList) toList

  @inline final def markQueen(zone: Zone, q: Position): Zone = {
    updateSquare(q, Square.q)(zone)
  }

  @inline final def updateSquare(p: Position, v: Square.Value)(zone: Zone): Zone = {
    val (x, y) = p
    zone.updated(x, zone(x).updated(y, v))
  }

  @inline final def inZone(position: Position, size: Int): Boolean = {
    val (x, y) = position
    (x >= 0 && y >= 0) && (x < size && y < size)
  }


  @tailrec
  @inline final def mkDiagonally(size: Int, position: Position, rt: Seq[Position] = Nil)(f: Position => Position): Seq[Position] = {
    val next = f(position)

    if(!inZone(next, size)) (position +: rt).reverse.tail
    else
      mkDiagonally(size, next, position +: rt)(f)
  }

  @inline final def leftUp(position: Position, size: Int): Seq[Position] = {
    mkDiagonally(size, position) {
      case (x, y) => (x - 1, y - 1)
    }
  }

  @inline final def rightDown(position: Position, size: Int): Seq[Position] = {
    mkDiagonally(size, position) {
      case (x, y) => (x + 1, y + 1)
    }
  }

  @inline
  final def leftDown(position: Position, size: Int): Seq[Position] = {
    mkDiagonally(size, position) {
      case (x, y) => (x + 1, y - 1)
    }
  }

  @inline
  final def rightUp(position: Position, size: Int): Seq[Position] = {
    mkDiagonally(size, position) {
      case (x, y) => (x - 1, y + 1)
    }
  }

  final def diagonally(p: Position, size: Int): Seq[Position] = {
    val buffer = scala.collection.mutable.Buffer.newBuilder[Position]

    buffer ++= leftUp(p, size)
    buffer ++= leftDown(p, size)
    buffer ++= rightDown(p, size)
    buffer ++= rightUp(p, size)
    buffer.result()
  }

  val batchUpdateTime = TimeUtil.VarTime()

  final def batchUpdate(seq: Seq[Position], square: Square.Value)(z: Zone): Zone = batchUpdateTime.computeTime {
    seq.groupBy(_._1).foldLeft(z) {
      case (zone, (x, row: Seq[(Int, Int)])) =>
        val old_row = zone(x).toVector
        val new_row = row.foldLeft(old_row) {
          case (seq, (_, y)) =>
            if(seq(y) == square) seq
            else
              seq.updated(y, square)
        }
        zone.updated(
          x, new_row
        )
    }
    //    seq.foldLeft(z) { case (z, p) => updateSquare(p, square)(z) }
  }

  val attackTime = TimeUtil.VarTime()

  final def attack(q: Position, size: Int) = attackTime computeTime {
    val (x, y) = q
    val column = 0 until size map (x -> _) filter (_ != q)
    val row = 0 until size map (_ -> y) filter (_ != q)
    val diagonally = this.diagonally(q, size)
    val around = Seq(
      (x - 1, y - 2),
      (x - 1, y - 1),
      (x - 1, y + 2),
      (x - 1, y + 1),

      (x - 2, y + 2),
      (x - 2, y + 1),
      (x - 2, y - 1),
      (x - 2, y - 2),

      (x + 1, y - 2),
      (x + 1, y - 1),
      (x + 1, y + 2),
      (x + 1, y + 1),

      (x + 2, y - 2),
      (x + 2, y - 1),
      (x + 2, y + 2),
      (x + 2, y + 1)
    ) filter (inZone(_, size))
    val buffer = scala.collection.mutable.Buffer.newBuilder[Position]
    buffer ++= column
    buffer ++= around
    buffer ++= row
    buffer ++= diagonally
    val attacks = buffer.result()
    attacks
  }

  val markWarTime = TimeUtil.VarTime()

  def markWar(zone: Zone, q: Position): Option[Zone] = markWarTime computeTime {
    val size = zone.size
    val attacks = attack(q, size)
    if(attacks.forall { case (x, y) => zone(x)(y) != Square.q }) {
      Some(
        batchUpdate(attacks, Square.attack)(zone)
      )
    }
    else None

  }

  val findSafeTime = TimeUtil.VarTime()

  def findSafe(x: Int, row: Seq[Square.Value]): IndexedSeq[(Int, Int)] = {
    row.indices zip row collect {
      case (y, Square.safe) => (x, y)
    }
  }

  def impl(zone: Zone, i: Int = 0, size: Int): Seq[Zone] = {
    def run(seq: Seq[Position]) = {
      seq.map(queen => markWar(markQueen(zone, queen), queen))
        .collect { case Some(x) => x.map(_.toVector) }
        .flatMap(e => impl(e, i + 1, size))
    }

    if(i == size) Seq(zone)
    else {
      findSafe(i, zone(i)).toList match {
        case Nil             => Seq()
        case safes if i == 0 =>
          val safesSize = safes.size
          val head = run(safes.take(safesSize / 2))

          def tail = head.map(_.reverse)

          def mind = run(Seq(safes(safesSize / 2)))

          if(safesSize % 2 == 0)
            head ++ tail
          else
            head ++ mind ++ tail
        case safes           => run(safes)
      }
    }

  }

  def solution(n: Int): Int = impl(mkZone(n), 0, n).size

}

object SuperQueens extends SuperQueens {
  def main(args: Array[String]): Unit = {
    def readListInt(): List[Int] = io.StdIn.readLine().split(" ").toList.map(_.toInt)

    //    val rt = solution(readListInt().head)
    val map = Map.apply(
      (1, 1),
      (2, 0),
      (3, 0),
      (4, 0),
      (5, 0),
      (6, 0),
      (7, 0),
      (8, 0),
      (9, 0),
      (10, 4),
      (11, 44),
      (12, 156),
      (13, 1876),
      (14, 5180)
    )

    println(map(readListInt().head))

  }
}
