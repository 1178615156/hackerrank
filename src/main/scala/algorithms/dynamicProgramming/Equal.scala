package algorithms.dynamicProgramming

object Equal {

  def special(seq: List[Long]): Long = {
    val a = seq.head
    val b = seq.last
    val diff = b - a
    val five = diff / 5
    val five_rest = diff % 5

    val len = seq.count(_ == b)

    if(five == 0) five_rest match {
      case 0 => 0
      case 1 => give(seq)
      case 2 => give(seq)
      case 3 => give(seq.head +: seq.tail.map(_ + 2)) + 1
      case 4 => give(seq.head +: seq.tail.map(_ + 1)) + 1
    } else five_rest match {
      case 0 => give(seq)
      case 1 => give(seq.head +: seq.tail.map(_ + 4)) + 2
      case 2 => give(seq.head +: seq.tail.map(_ + 3)) + 2
      case 3 => give(seq.head +: seq.tail.map(_ + 2)) + 1
      case 4 => give(seq.head +: seq.tail.map(_ + 1)) + 1
    }


  }

  //1 5 7
  //1 6 8 -1
  //6 6 13 -1
  //13 13 13 -2

  def give(seq: List[Long], plus: Long = 0L, acc_rt: Long = 0L): Long = seq match {
    case Nil             => acc_rt
    case a :: Nil        => acc_rt
    case a :: b :: other =>
      val diff = b - a + plus

      val five = diff / 5
      val five_rest = diff % 5


      val two = five_rest / 2
      val two_rest = five_rest % 2

      val one = two_rest
      val result = five + two + one
      five_rest match {
        case 3 if other.contains(b) =>
          math.min(
            give((b + plus) :: other, plus + diff, acc_rt + result),
            give(seq.head +: seq.tail.map(_ + 2), plus, acc_rt + 1)
          )
        case 4 if other.contains(b) =>
          math.min(
            give((b + plus) :: other, plus + diff, acc_rt + result),
            give(seq.head +: seq.tail.map(_ + 1), plus, acc_rt + 1)
          )
        case _                      => give((b + plus) :: other, plus + diff, acc_rt + result)
      }
  }


  def give(seq: Seq[Long], min: Long) = {
    seq.map { e =>
      val diff = e - min

      val five = diff / 5
      val five_rest = diff % 5

      val two = five_rest / 2
      val two_rest = five_rest % 2

      val one = two_rest
      val result = five + two + one

      result
    }.sum
  }

  def solution(seq: Seq[Long]) = {
    val min = seq.min
    (0 until 5).map(base => give(seq, min - base)).min
  }

  def readListInt() = scala.io.StdIn.readLine().split(" ").toList.map(_.toLong)

  def main(args: Array[String]): Unit = {
    val t :: Nil = readListInt()

    val data = 1L to t map (_ => {
      readListInt()
      readListInt()
    })
    val result = data.map(solution)
    println(result.mkString("\n"))
  }
}
