package functionalProgramming.memoization

/**
  * Created by yujieshui on 2017/2/17.
  */
case class Dice(top: Int, front: Int, right: Int) {
  val back   = 7 - front
  val bottom = 7 - top
  val left   = 7 - right
}

object Dice {
  val topOne = Dice(1, 2, 4)

  def right(dice: Dice): Dice =
    Dice(top = dice.left, front = dice.front, right = dice.top)

  def down(dice: Dice): Dice =
    Dice(top = dice.back, front = dice.top, right = dice.right)
}

class DicPath {
  def impl(i_m: Int, i_n: Int, dice: Dice)(m: Int, n: Int): Int = {
    if(i_m == m && i_n == n) dice.top
    else if(math.abs(i_m - i_n) > math.abs(m - n) + 1) 0
    else if(i_m == m) dice.top + impl(i_m, i_n + 1, Dice.right(dice))(m, n)
    else if(i_n == n) dice.top + impl(i_m + 1, i_n, Dice.down(dice))(m, n)
    else math.max(
      dice.top + impl(i_m, i_n + 1, Dice.right(dice))(m, n),
      dice.top + impl(i_m + 1, i_n, Dice.down(dice))(m, n)
    )
  }

  def solution(m: Int, n: Int) = {
    impl(1, 1, Dice.topOne)(m, n)
  }

}


object DicPath extends DicPath {
  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val t :: Nil = readListInt()
    val data = 1 to t map (_ => readListInt())
    val result = data map (e => solution(e(0), e(1)))

    println(result.mkString("\n"))
  }
}

































