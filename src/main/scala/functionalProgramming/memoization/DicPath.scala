package functionalProgramming.memoization

/**
  * Created by yujieshui on 2017/2/17.
  */
case class Dice(top: Int, front: Int, right: Int) {
  val back   = 7 - front
  val bottom = 7 - top
  val left   = 7 - right

  override def toString: String = top.toString
}

object Dice {
  val topOne = Dice(1, 2, 4)

  def right(dice: Dice): Dice =
    Dice(top = dice.left, front = dice.front, right = dice.top)

  def down(dice: Dice): Dice =
    Dice(top = dice.back, front = dice.top, right = dice.right)
}

class DicPath {
  def impl(i_m: Int, i_n: Int, dice: Dice, acc: Int)(m: Int, n: Int): Int = {
    if(i_m == m && i_n == n) dice.top + acc
    else if(i_m == m) impl(i_m, i_n + 1, Dice.right(dice), acc + dice.top)(m, n)
    else if(i_n == n) impl(i_m + 1, i_n, Dice.down(dice), acc + dice.top)(m, n)
    else math.max(
      impl(i_m, i_n + 1, Dice.right(dice), acc + dice.top)(m, n),
      impl(i_m + 1, i_n, Dice.down(dice), acc + dice.top)(m, n)
    )
  }

  def mkCache(m: Int, n: Int) = {
    val pos = for {
      im <- 1 to m
      in <- 1 to n
    } yield (im, in)

    pos.tail.foldLeft(Map[(Int, Int), (Int, Dice)]((1, 1) -> (1, Dice.topOne))) {
      case (map, (1, in)) =>
        val right = map((1, in - 1))
        val (max, dice) = right
        val new_dice = Dice.right(dice)
        val new_max = max + new_dice.top
        map + ((1, in) -> (new_max -> new_dice))

      case (map, (im, 1)) =>
        val down = map((im - 1, 1))
        val (max, dice) = down
        val new_dice = Dice.down(dice)
        val new_max = max + new_dice.top
        map + ((im, 1) -> (new_max -> new_dice))

      case (map, (im, in)) =>
        val (right_max, right_dice) = map(im, in - 1)
        val (down_max, down_dice) = map(im - 1, in)
        if(right_max + Dice.right(right_dice).top > down_max + Dice.down(down_dice).top)
          map + ((im, in) -> (
            right_max + Dice.right(right_dice).top, Dice.right(right_dice)
          ))
        else
          map + ((im, in) -> (
            down_max + Dice.down(down_dice).top, Dice.down(down_dice)
          ))
    }
  }

  val cache = mkCache(3, 3)


  def solution(m: Int, n: Int) = {
    //    impl(1, 1, Dice.topOne, 0)(m, n)
    cache(m, n)._1
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

































