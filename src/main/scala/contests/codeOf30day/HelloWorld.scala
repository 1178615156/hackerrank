package contests.codeOf30day

/**
  * Created by yuJieShui on 2016/1/1.
  */
object HelloWorld {
  def main(args: Array[String]) {
    println(
      s"""Hello World.
          |Welcome to 30 Days of Code.
       """.stripMargin)
  }
}

object Arithmetic {
  def main(args: Array[String]) {
    val m = io.StdIn.readDouble()
    val t = io.StdIn.readInt()
    val x = io.StdIn.readInt()
    val tip = t.toDouble * m / 100
    val tax = x.toDouble * m / 100
    println("The final price of the meal is $" + (m + tip + tax).toInt + ".")
  }
}