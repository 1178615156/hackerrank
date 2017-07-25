package algorithms.dynamicProgramming

import org.scalatest.{FunSuite, WordSpec}
import HyperStrings._

/**
  * Created by yujieshui on 2017/7/20.
  */
class HyperStringsTest extends WordSpec {


  "decomposition" must {
    "1" in {
      println(decomposition(4).map(_.sorted).toSet.mkString("\n"))
    }

  }
  "decompositionCache" must {
    val cache: Map[Int, Set[Seq[Int]]] = Map(0 -> Set(Seq()), 1 -> Set(Seq(1)))
    "4" in {
      println(decompositionCache(1, 4, cache).toSeq.mkString("\n"))
    }
    "10" in {
      println(decompositionCache(1, 10, cache).mkString("\n"))

    }
    "100" in {
      println(decompositionCache(1, 100, cache).mkString("\n"))
    }
  }

  "solution " must {
    "case 0" in {
      val result = solution(3, Seq("a", "ab"))
      println(result)
    }
    "case 1" in {
      val result = solution(3, Seq("a", "b", "c"))
      println(result)

    }
  }
}
