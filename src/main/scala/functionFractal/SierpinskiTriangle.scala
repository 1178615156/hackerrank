package functionFractal

/**
  * Created by yuJieShui on 2016/1/2.
  */
object PascalTriangle {
  def stratum(n: Int): Int = {
    if (n == 0)
      1
    else
      n * stratum(n - 1)
  }

  def c(m: Int, n: Int) = {
    (stratum(m).toDouble / (stratum(n) * stratum(m - n))).toInt
  }

  def main(args: Array[String]) {
    val n = io.StdIn.readInt()


    val out = (0 until n) map { e ⇒
      0 to e map (i ⇒ c(e, i))
    }
    println(out.map(e ⇒ e.mkString(" ")).mkString("\n"))
  }
}

//noinspection ScalaDeprecation
object SierpinskiTriangle {
  type Triangle = Seq[String]

  def mkTriangle(max: Int, char: String): Triangle =
    (1 to (max - 1)).foldLeft(List[Int](1)) { (l, r) ⇒
      l.:+(l.last + 2)
    } map (char * _)

  def reverseTriangle(triangle: Triangle): Triangle =
    triangle.reverse.map("_" * _.size)

  def drawTriangles(n: Int, triangleSize: Int): Triangle = {
    if (n == 1) mkTriangle(triangleSize, "1")
    else {
      val upTriangle = drawTriangles(n - 1, triangleSize)
      val drawReverseTriangle = reverseTriangle(upTriangle)
      val downTriangle = (upTriangle, drawReverseTriangle, upTriangle).zipped map {
        (l, m, r) ⇒ l + m + r
      }
      upTriangle ++ downTriangle
    }
  }

  def drawTriangles(n: Int) {
    //Draw the N'th iteration of the fractal as described
    // in the problem statement
    val s = 0 to 31 map ("_" * _)
    val m: Triangle = drawTriangles(n + 1, (0 to 5 map (math.pow(2, _).toInt)).reverse apply (n + 1 - 1))
    val out = (s.reverse, m, s.reverse).zipped.map(
      (l, m, r) ⇒ l + m + r
    )
    println(out.mkString("\n"))
  }

  def main(args: Array[String]) {

    drawTriangles(readInt())

  }
}


object StringMingling {
  def main(args: Array[String]) {
    val a = io.StdIn.readLine().map(_.toString)
    val b = io.StdIn.readLine().map(_.toString)
    val out =a zip b map {
      case (a,b)⇒ a + b
    }
    println(out.mkString)
  }
}
object StringPermute{
  def main(args: Array[String]) {
    val n = io.StdIn.readLine().toInt
     1 to n map {_ ⇒ }

    val b = io.StdIn.readLine()
  }
}