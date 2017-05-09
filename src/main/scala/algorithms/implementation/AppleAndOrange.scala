package algorithms.implementation

/**
  * Created by yujieshui on 2017/5/9.
  */
object AppleAndOrange {
  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  case class Entity(s: Int, t: Int, a: Int, b: Int)

  def solution(entity: Entity, apples: Seq[Int], oranges: Seq[Int]) = {
    val appleNum = apples.map(_ + entity.a).count(i => entity.s <= i && i <= entity.t)
    val orangeNum = oranges.map(_ + entity.b).count(i => entity.s <= i && i <= entity.t)
    appleNum -> orangeNum
  }

  def main(args: Array[String]): Unit = {
    val s :: t :: Nil = readListInt()
    val a :: b :: Nil = readListInt()
    val n :: m :: Nil = readListInt()
    val apples = readListInt()
    val oranges = readListInt()
    val (appleNum, orangeNum) = solution(Entity(s, t, a, b), apples, oranges)
    println(appleNum)
    println(orangeNum)
  }
}
