package algorithms.gameTheory

/**
  * Created by yujieshui on 2016/8/12.
  */
object TowerBreakersRevisited {

  type Height = Int
  type Towers = Seq[Height]

  def primeTable(limit: Int): Seq[Height] = {
    val init = Vector(7, 5, 3, 2)
    (8 to limit).foldLeft(init) {
      case (acc, i) if init.slice(init.size - math.sqrt(i).toInt - 1, init.size).exists(i % _ == 0) => acc

      case (acc, i) => i +: acc
    }.reverse
  }

  val initPrimeTable = primeTable(1000000)


  def count_prime_factor(height: Height): Int = {
    if (height == 1) 0
    else {
      initPrimeTable.find(e => height % e == 0) match {
        case Some(e) => 1 + count_prime_factor(height / e)
        case None    => 0
      }
    }

  }

  def solution(towers: Towers) = {
    val result = towers map count_prime_factor reduceLeft (_ ^ _)
    if (result > 0) "1" else "2"
  }

  def main(args: Array[String]): Unit = {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

    val n :: Nil = readListInt()
    val in = 1 to n map (_ => {readListInt();readListInt()})

    val result = in map solution

    println(result mkString "\n")
  }
}
