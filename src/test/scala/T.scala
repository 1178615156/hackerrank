/**
  * Created by yuJieShui on 2016/1/2.
  */
class T extends org.scalatest.FunSuite {
  test("") {
    //    val x = 63
    //    val y = 32
    type Triangle = Seq[String]
    def mkTiangle(max: Int, char: String): Triangle =
      1 to (max, 2) map (char * _)

    def reverseTriangle(triangle: Triangle): Triangle =
      triangle.reverse.map("_" * _.size)

    def dr(n: Int) = {
      assert(n > 0 && n <= 5)
      val triangleSize = 32 >> (n - 1)
      val drawTriangle = mkTiangle(triangleSize, "1")
      val drawReverseTriangle = reverseTriangle(drawTriangle)

    }
    //mkTiangle(4, "1")
    //reverseTriangle(mkTiangle(4, "1"))
    //dr(4)

    ((0 - 1) to (4 - 1)).toList.foldLeft(List[Int]()) { (l: List[Int], r: Int) ⇒
      l.:+(l.last + 2)
    }
  }
  test("mk triangle") {
    type Triangle = Seq[String]
    def mkTiangle(max: Int, char: String): Triangle =
      (1 to (max - 1)).foldLeft(List[Int](1)) { (l, r) ⇒
        l.:+(l.last + 2)
      } map (char * _)

    def reverseTriangle(triangle: Triangle): Triangle =
      triangle.reverse.map("_" * _.size)

    def drawTriangle(n: Int) = {
      val triangleSize = (1 to 5 map (math.pow(2, _).toInt)).reverse apply (n - 1)
      if (n == 1) mkTiangle(triangleSize, "1")
      else {

      }
    }
    def dr(n: Int, baseTriangle: Triangle = Nil): Seq[String] = {
      val triangleSize = (1 to 5 map (math.pow(2, _).toInt)).reverse apply (n - 1)
      val drawTriangle: Triangle =
        if (n == 1) mkTiangle(triangleSize, "1")
        else if (n == 2) dr(4)
        else if (n == 3) dr(3)
        else if (n == 4) dr(2)
        else if (n == 5) mkTiangle(triangleSize, "1")
        else ???

      val drawReverseTriangle = reverseTriangle(drawTriangle)
      def upTriangle = drawTriangle
      def downTriangle = (drawTriangle, drawReverseTriangle, drawTriangle).zipped map {
        (l, m, r) ⇒ l + m + r
      }
      if (n == 1)
        drawTriangle
      else
        upTriangle ++ downTriangle

    }
    println(
      dr(3) mkString ("\n", "\n", "\n")
    )

  }

  test("t2") {

    type Triangle = Seq[String]

    def mkTiangle(max: Int, char: String): Triangle =
      (1 to (max - 1)).foldLeft(List[Int](1)) { (l, r) ⇒
        l.:+(l.last + 2)
      } map (char * _)

    def reverseTriangle(triangle: Triangle): Triangle =
      triangle.reverse.map("_" * _.size)

    def drawTriangles(n: Int, triangleSize: Int): Triangle = {
      if (n == 1) mkTiangle(triangleSize, "1")
      else {
        val upTriangle = drawTriangles(n - 1, triangleSize)
        val drawReverseTriangle = reverseTriangle(upTriangle)
        val downTriangle = (upTriangle, drawReverseTriangle, upTriangle).zipped map {
          (l, m, r) ⇒ l + m + r
        }
        upTriangle ++ downTriangle

      }
    }

    val out =
      List(
        drawTriangles(1, (1 to 5 map (math.pow(2, _).toInt)).reverse apply (1 - 1)).mkString("\n", "\n", "\n"),
        drawTriangles(2, (1 to 5 map (math.pow(2, _).toInt)).reverse apply (2 - 1)).mkString("\n", "\n", "\n"),
        drawTriangles(3, (1 to 5 map (math.pow(2, _).toInt)).reverse apply (3 - 1)).mkString("\n", "\n", "\n"),
        drawTriangles(4, (1 to 5 map (math.pow(2, _).toInt)).reverse apply (4 - 1)).mkString("\n", "\n", "\n"),
        drawTriangles(5, (1 to 5 map (math.pow(2, _).toInt)).reverse apply (5 - 1)).mkString("\n", "\n", "\n")
      )

    println(out)
  }
  //  if (n == 5) {
  //    val drawTriangle = mkTiangle(triangleSize, "1")
  //    val drawReverseTriangle = reverseTriangle(drawTriangle)
  //    def upTriangle = drawTriangle
  //    def downTriangle = (drawTriangle, drawReverseTriangle, drawTriangle).zipped map {
  //      (l, m, r) ⇒ l + m + r
  //    }
  //    upTriangle ++ downTriangle
  //  }
  //  else {
  //    val up = dr(n + 1)
  //    val drawTriangle = up
  //    val drawReverseTriangle = reverseTriangle(drawTriangle)
  //    def upTriangle = drawTriangle
  //    def downTriangle = (drawTriangle, drawReverseTriangle, drawTriangle).zipped map {
  //      (l, m, r) ⇒ l + m + r
  //    }
  //    upTriangle ++ downTriangle
  //  }
}
