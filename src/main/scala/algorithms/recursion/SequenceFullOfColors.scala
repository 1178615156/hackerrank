package algorithms.recursion


object SequenceFullOfColors {

  object Color extends Enumeration {
    val R = Value("R")
    val G = Value("G")
    val Y = Value("Y")
    val B = Value("B")
  }

  type Color = Color.Value

  def solution(seq: Seq[Color]) = {
    val colorSizeMap = seq.groupBy(e => e).mapValues(_.size)

    val r_size = colorSizeMap.getOrElse(Color.R, 0)
    val g_size = colorSizeMap.getOrElse(Color.G, 0)
    val y_size = colorSizeMap.getOrElse(Color.Y, 0)
    val b_size = colorSizeMap.getOrElse(Color.B, 0)

    r_size == g_size && y_size == b_size && differencePrefix(seq.toList, 0, 0, 0, 0)

  }

  def differencePrefix(seq: List[Color], r: Int, g: Int, y: Int, b: Int): Boolean = {
    if(math.abs(r - g) > 1 || math.abs(y - b) > 1) false
    else seq match {
      case Nil             => true
      case Color.R :: tail => differencePrefix(tail, r + 1, g, y, b)
      case Color.G :: tail => differencePrefix(tail, r, g + 1, y, b)
      case Color.Y :: tail => differencePrefix(tail, r, g, y + 1, b)
      case Color.B :: tail => differencePrefix(tail, r, g, y, b + 1)
    }
  }

  def string2colors(string: String): List[Color] = {
    string.toList.map(_.toString).map(Color.withName)
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: Nil = readListInt()
    val inputs = 1 to n map (_ => io.StdIn.readLine())

    val results = inputs.map(string2colors).map(solution)

    val outputs = results.map {
      case true  => "True"
      case false => "False"
    }

    println(outputs.mkString("\n"))
  }
}
