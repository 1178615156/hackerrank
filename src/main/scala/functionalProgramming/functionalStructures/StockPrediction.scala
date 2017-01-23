package functionalProgramming.functionalStructures


/**
  * Created by yujieshui on 2017/1/23.
  */

class StockPrediction {


  type Contain[T] = Seq[T]

  case class Limit(min: Long, max: Long)

  case class Entity(head: Contain[Long], value: Long, tail: Contain[Long],
                    i: Int, min: Long, max: Long, total: Int)

  type Data = Array[Entity]

  def mkData(seq: Contain[Long]): Data = {
    val max = seq.max
    val min = seq.min
    val total = seq.size

    def mkSeq(head: Seq[Long], tail: Seq[Long], i: Int, result: Seq[Entity]): Seq[Entity] = {
      if(tail.isEmpty) result.reverse
      else mkSeq(
        tail.head +: head,
        tail.tail,
        i + 1,
        Entity(head, tail.head, tail.tail, i, min, max, total) +: result
      )
    }

    val result = mkSeq(Nil, seq.toList, 0, Seq())
    result.toArray
  }

  def mkLimit(d: Long, m: Long)(entity: Entity): Limit = {
    val min = entity.value
    val max = min + m
    Limit(min, max)
  }

  def countInLimit(limit: Limit, entity: Entity): Long = {
    val Limit(min, max) = limit

    def f(e: Long) = min <= e && e <= max

    def countHead(seq: Seq[Long], i: Int): Int =
      if(seq.isEmpty || !f(seq.head)) i else countHead(seq.tail, i + 1)

    if(f(entity.min) && f(entity.max)) entity.total else {
      val hs = countHead(entity.head, 0)
      val ts = countHead(entity.tail, 0)
      hs + ts + 1
    }

  }

  def solution(seq: Contain[Long], q: Seq[(Long, Long)]): Seq[Long] = {
    val data = mkData(seq)
    q map { case (d, m) =>
      val entity = data(d.toInt)
      val limit = mkLimit(d, m)(entity)
      countInLimit(limit, entity)
    }

  }
}

object StockPrediction extends StockPrediction {
  def readListLong(): List[Long] = io.StdIn.readLine().split(" ").map(_.toLong).toList

  def main(args: Array[String]): Unit = {
    readListLong()
    val array = readListLong()
    val q_n = readListLong().head
    val qs = 1L to q_n map { _ =>
      val d :: m :: Nil = readListLong()
      (d, m)
    }
    println(
      solution(array, qs).mkString("\n")
    )
  }
}
