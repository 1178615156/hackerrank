package functionalProgramming.memoization

/**
  * Created by yujieshui on 2016/8/22.
  */
object BitterChocolate {
  type Row = Int
  type Chocolate = Seq[Row]

  object Play extends Enumeration {
    val First  = Value("First")
    val Second = Value("Second")
    type Play = Value

    def switchPlay(play: Play) = play match {
      case First  => Second
      case Second => First
    }
  }

  import Play._

  def allCanEat(chocolate: Chocolate): Seq[Chocolate] = ???

  def eat(row: Row, num: Int)(chocolate: Chocolate): Chocolate = ???

  def asBitter(chocolate: Chocolate) = chocolate.size == 1 && chocolate.head == 1

  def solution(chocolate: Chocolate, play: Play): Play = {
    if (asBitter(chocolate))
      switchPlay(play)
    else if (chocolate.size == 1 || chocolate.forall(_ == 1))
      play
    else {
      val win =
        allCanEat(chocolate).exists { nextChocolate =>
          solution(nextChocolate, switchPlay(play)) == play
        }
      if (win) play else switchPlay(play)
    }
  }
}
