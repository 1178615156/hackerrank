package algorithms.dynamicProgramming

import org.scalatest.{FunSuite, WordSpec}
import HyperStrings._

/**
  * Created by yujieshui on 2017/7/20.
  */
class HyperStringsTest extends WordSpec {


  "decomposition" must {
    "1" in {
      println(decomposition( 30).mkString("\n"))
    }

  }
  "decompositionCache" must {
    "1" in {
      val cache: Map[Int, Set[ Seq[Int]]] = Map(0 -> Set(Seq()), 1 -> Set(Seq(1)))
      println(decompositionCache(1, 100, cache).mkString("\n"))
    }
  }
}
