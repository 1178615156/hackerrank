import utils.SetInt

object BearAndWorkbook {
  def readSeqInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]) {
    val chapterNum :: chapterMaxProblemNum :: Nil = readSeqInt()
    val problems = readSeqInt()

    val pages = problems.flatMap(problemNum => {
      type Contain = Seq[Int]
      def toPage(problems: Contain, rt: Seq[Contain] = Nil): Seq[Contain] = {
        val tail = problems drop chapterMaxProblemNum
        def result: Seq[Contain] = rt :+ (problems take chapterMaxProblemNum)
        if (tail.isEmpty)
          result
        else
          toPage(tail, result)
      }
      toPage(1 to problemNum)
    })
  //  println(pages.indices map (_ + 1) zip pages)

    println(pages.indices map (_ + 1) zip pages count {
      case (index, page) =>
        page contains index
    })
  }


  SetInt(
    """40 7
      |1 10 12 4 11
    """.stripMargin)
}
