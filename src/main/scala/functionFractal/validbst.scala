package functionFractal

import utils.SetInt

import scala.language.implicitConversions

/**
  * Created by yujieshui on 2016/3/14.
  */
object validbst {


  implicit def bbbb[T](binaryTree: BinaryTree[T]) = {
    if (binaryTree.isEmpty)
      None
    else
      Some(binaryTree)
  }

  case class BinaryTree[T](
                            value: Option[T] = None,
                            left: Option[BinaryTree[T]] = None,
                            right: Option[BinaryTree[T]] = None
                          ) {
    def isEmpty = false

    override def toString: String = {
      s"""{value:$value, left:$left , right:$right}"""
    }
  }

  object BinaryTreeEmpty {
    def apply[T](): BinaryTree[T] = {
      new BinaryTree[T]() {
        override def toString = "emptyTree"

        override def isEmpty = true
      }
    }
  }

  def list2binaryTree(list: List[Int]): BinaryTree[Int] = {
    if (list.isEmpty)
      BinaryTreeEmpty[Int]()
    else {
      val root_value = list.head
      def subTree(rt: (List[Int], List[Int])): (List[Int], List[Int]) = {
        val (left, right) = rt

        if (right.isEmpty || right.head > root_value)
          rt
        else
          subTree((left :+ right.head, right.tail))
      }
      val (left, right) = subTree(Nil -> list.tail)

      BinaryTree(Some(root_value), list2binaryTree(left), list2binaryTree(right))
    }

  }

  def depthSearch(binaryTree: BinaryTree[Int]): List[Option[Int]] = {
    if (binaryTree.isEmpty)
      Nil
    else {
      val l = depthSearch(binaryTree.left.getOrElse(BinaryTreeEmpty()))
      val r = depthSearch(binaryTree.right.getOrElse(BinaryTreeEmpty()))
      (l :+ binaryTree.value) ++ r
    }

  }

  def valid(binaryTree: BinaryTree[Int]): Boolean = {
    val l = depthSearch(binaryTree)
    val ll = l.collect { case Some(x) => x }

    def isSort(ll: List[Int], n: Int): Boolean = {
      if (ll.isEmpty)
        true
      else {
        if (ll.head >= n)
          isSort(ll.tail, ll.head)
        else
          false
      }

    }
    isSort(ll, ll.head)
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]) {
    val n :: Nil = readListInt()
    val data = 1 to n map {
      _ =>
        readListInt()
        readListInt()
    }
    val result =
      data.map(e => valid(list2binaryTree(e)))
          .map{
            case true => "YES"
            case false => "NO"
          }
        .mkString("\n")

    println(result)
  }

  SetInt(
    """5
      |3
      |1 2 3
      |3
      |2 1 3
      |6
      |3 2 1 5 4 6
      |4
      |1 3 4 2
      |5
      |3 4 5 1 2
    """.stripMargin)
}
