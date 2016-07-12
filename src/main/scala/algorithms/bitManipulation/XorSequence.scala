package algorithms.bitManipulation

/**
  *
  */

object XorSequence {
  @inline final def xor(p: Long, q: Long): Long = p ^ q

  implicit final class WithXor(val p: Int) extends AnyVal {
    @inline def xor(q: Int) = p ^ q
  }

  val bitList             = 0 to 62 map (i => 1l << i)
  val defaultXorHeadArray = {
    Seq.iterate(0l -> 0l, 100000) {
      case (value, index) => xor(value, index + 1) -> (index + 1)
    }.map(_._1)
  }

  def xorStartNum(n: Long): Long = {
    if (n <= 4) defaultXorHeadArray(n.toInt)
    else {
      val max2Bit = bitList.filter(_ <= n).last
      if ((n - max2Bit) % 2 == 0)
        xor(max2Bit, xorStartNum(n - max2Bit))
      else
        xorStartNum(n - max2Bit)
    }
  }

  def compute(start: Long, end: Long) = {
    sliceOf(start, end + 1).reduceLeft(xor)
  }

  def sliceOf(start: Long, end: Long) = {
    Seq.iterate(xorStartNum(start) -> start, (end - start).toInt) {
      case (value, index) =>
        xor(value, index + 1) -> (index + 1)
    }.map(_._1)
  }

  def main(args: Array[String]) {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toLong)
    val n :: Nil = readListInt()
    val data = 1l to n map (_ => {
      val start :: end :: Nil = readListInt()
      start -> end
    })

    val out =
      data.map { case (start, end) => compute(start, end) }.mkString("\n")

    println(out)
  }
}
