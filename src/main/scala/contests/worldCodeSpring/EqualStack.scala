package contests.worldCodeSpring

/**
  * Created by yuJieShui on 2016/6/26.
  */
object EqualStack {
  case class Stack(seq: Seq[Int],sum:Int){
    def tail = Stack(seq.tail,sum - seq.head)
  }
  type Stacks = Seq[Stack]
  type Height = Int

  def height(stack: Stack): Height = stack.sum

  def equal_stack_height(stacks: Stacks) = {
    val a = height(stacks.head)
    stacks.forall(_.sum == a)
  }


  def solution(stacks: Seq[Stack]): Height = {
    if (equal_stack_height(stacks))
      height(stacks.head)
    else {
      val stackSortByHeight =
        stacks.sortBy(_.sum)

      solution(stackSortByHeight.init :+ stackSortByHeight.last.tail)
    }
  }

  def main(args: Array[String]) {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val sizeOfStack = readListInt()
    val data = sizeOfStack map {_ =>
      readListInt()
    } map (seq => Stack(seq,seq.sum ))
    println(
      solution(data)
    )
  }
}
