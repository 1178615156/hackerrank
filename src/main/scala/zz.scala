import utils.SetInt

/**
  * Created by yuJieShui on 2016/3/20.
  */
trait A
trait B
trait AB extends A with B
object zz {
  def main(args: Array[String]) {
    val in = List(
      "ab",
      "abc",
      "abf",
      "abcg"
    )

    def deduplication(list: List[String]) = {
      list.sorted.reverse.foldLeft(List.empty[String]) { (result, r) =>
        if (result.headOption.exists(e => e.contains(r)))
          result
        else
          r +: result
      }.reverse
    }
    val result = deduplication(in)

    println(result)
  }

  final var a = 1

  a +=1
  SetInt(
    """
      |
      |
      |
    """.stripMargin)
}
