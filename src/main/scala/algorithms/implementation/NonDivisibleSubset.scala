package algorithms.implementation

/**
  * Created by yujieshui on 2017/5/10.
  */
object NonDivisibleSubset {
  def sumDivisible(k: Int): Seq[(Int, Int)] = for {
    a <- 0 to k
    b <- 0 to k if (a + b) % k == 0
  } yield
    (a, b)

  def impl(seq: Seq[Int], sd: Set[(Int, Int)], rt: Seq[Set[Int]],pass:Seq[Set[Int]]= Seq()): Seq[Set[Int]] = {
    val result = rt.flatMap { (e: Set[Int]) =>
      val x: Seq[Set[Int]] = seq
        .filter(i =>
          e.forall(t => t != i && !sd.contains(t -> i)))
        .map(i => e + i)
      x
    }.distinct
    if(result.isEmpty) rt ++ pass else impl(seq, sd, result,rt ++ pass)
  }

  def solution(k: Int, arr: Seq[Int]) = {
    val seq = arr.groupBy(e => e % k).mapValues(_.length)
    impl(seq.keys.toSeq, sumDivisible(k).toSet, Seq(Set()))
      .map { result =>
        result.map(e => if((e * 2) % k == 0) 1 else seq(e))
          .sum
      }.max
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: k :: Nil = readListInt()
    val data = readListInt()
    val result = solution(k, data)
    println(result)
  }
}
