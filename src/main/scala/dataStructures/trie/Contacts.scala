package dataStructures.trie

/**
  * Created by yujieshui on 2016/7/27.
  */
object Contacts {

  trait Contact

  type Tree = Map[Char, Contact]

  object End

  case class Next(tree: Tree)

  def solution(tree: Tree,findPartial:Seq[Char]) = {
    findPartial match {
      case head :: other =>
        tree.get(head)
    }
    ???
  }
}
