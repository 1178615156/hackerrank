package functionFractal.recursion

import scala.collection.immutable.IndexedSeq

/**
  * Created by yujieshui on 2016/3/15.
  */
object Crossword {
  val start = 1
  val end   = 10

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  val + = '+'
  val - = '-'
  val o = 'o'

  def asWordPesion(list: Seq[Seq[Char]]) = {
    list.indices zip list map {
      case (x, lend) => lend.indices zip lend map {
        case (y, element) =>
          element match {
            case `+` =>
            case `-` =>
            case `o` =>
          }
      }
    }
  }

  def main(args: Array[String]) {
    val in = 1 to 10 map (_ => io.StdIn.readLine())
    val inData: IndexedSeq[List[Option[Char]]] = in map (_.toList.map {
      case '+' => Some('+')
      case '-' => None
    })
    val words = io.StdIn.readLine().split(";").toSeq
  }
}
