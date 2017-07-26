package functionalProgramming.persistent

import org.scalatest.{FunSuite, WordSpec}

/**
  * Created by yujieshui on 2017/7/22.
  */
class BoleynSalaryTest extends WordSpec {

  import BoleynSalary._

  val salary                         = "70 40 60 80 10 20 30 50".split(" ").map(_.toInt)
  val relation                       = Seq(
    Relation(2, 1),
    Relation(3, 2),
    Relation(4, 2),
    Relation(7, 4),
    Relation(8, 4),
    Relation(5, 1),
    Relation(6, 5)
  )
  val tree                           = mkTree(
    1,
    relation2map(relation),
    (1 to salary.size zip salary).toMap
  )
  val tii: Map[Salary, Tree[Employ]] = treeIdIndex(
    tree
  ).toMap
  "mkTree" must {
    "1" in {

      println(tree)
    }
  }
  "treeIdIndex" in {
    println(tii.map(e => e._1))
  }

  "solution" must {
    "2->1" in {
      assert(solution(relation, (1 to salary.size zip salary).toMap, Seq(2 -> 1)) === Seq(7))
    }
    "1->5" in {
      assert(solution(relation, (1 to salary.size zip salary).toMap, Seq(1 -> 5)) === Seq(8))
    }

  }
}
