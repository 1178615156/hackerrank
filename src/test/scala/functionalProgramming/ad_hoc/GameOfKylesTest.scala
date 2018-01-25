package functionalProgramming.ad_hoc

import functionalProgramming.ad_hoc
import org.scalatest.{FunSuite, WordSpecLike}

/**
  * Created by yujieshui on 2017/1/7.
  */
class GameOfKylesTest extends WordSpecLike {

  import GameOfKyles._

  implicit val f: (Seq[Pin]) => Set[ad_hoc.GameOfKyles.Pin] = seq2set _

  "enum" must {
    "1" in {
      assert(playGame(Seq(1)))
    }
    "2" in {
      assert(playGame(Seq(2)))
    }
    "3" in {
      assert(playGame(Seq(3)))
    }
    "10" in {
      1 to 10 foreach (i => assert(playGame(Seq(i)), i))
    }
    "1,2" in {
      assert(playGame(Seq(1, 2)))
    }
    "1,1,1" in {
      val seq = Seq(1, 1, 1)
      assert(playGame(seq))
    }
    "1,2,1" in {
      val seq = Seq(1, 2, 1)
      assert(playGame(seq))
    }
    "1,3,1" in {
      val seq = Seq(1, 3, 1)
      assert(playGame(seq))
    }
    "1,2,2" in {
      val seq = Seq(1, 2, 2)
      assert(playGame(seq))
    }
    "n,n" in {
      1 to 6 foreach (i => assert(!playGame(Seq(i, i)), i))
    }
    "1,2,3" in {
      assert(!playGame(Seq(1, 2, 3)))
    }
    "2,3,4" in {
      assert(!playGame(Seq(2, 3, 4)))
    }
    "1,2,3,4" in {
      assert(playGame(Seq(1, 2, 3, 4)))
    }
    "1,2,3,4,5" in {
      assert(playGame(Seq(1, 2, 3, 4, 5)))
    }
    "4,4,10" in {
      val seq = Seq(4, 4, 10)
      assert(playGame(seq))
    }
    "12,34,56" in {
      //      val seq = Seq(12, 34, 56)
      //      assert(playGame(seq))
    }
    "finish" in {
      println(result.mkString("\n"))
    }
  }

  "prode" must {
    "10 10" in {
      val x = for {
        a <- 1 to 20
        b <- 1 to 20
      } yield
        ((a, b), playGame(Seq(a, b)),Integer toBinaryString a | b , Integer.toBinaryString(a ^ b)  )
      //      println(x.filter(_._2 == false).map(_._1).map(_._1))
      //      println(x.filter(_._2 == false).map(_._1).map(_._2))
      println(x.mkString("\n"))

    }
  }

  "test case " must {
    "1" in {

      assert(playGame(line2Seq("IIXXIIIIII")))
      assert(playGame(line2Seq("IXIXI")))
      assert(!playGame(line2Seq("XXIXXI")))
      assert(!playGame(line2Seq("IIXII")))
      assert(playGame(line2Seq("XIXIIXII")))
      assert(playGame(line2Seq("IIXIII")))
      assert(playGame(line2Seq("IXXXX")))
      assert(playGame(line2Seq("IXIXIII")))
      assert(playGame(line2Seq("XIIIXIXXIX")))
    }
    "xx" in {
      assert(playGame(Seq(7, 8, 9, 10)))
      assert(playGame(Seq(3, 5, 5, 10)))

    }
    "3" in {
      val x = for {
        a <- 1 to 20
        b <- (a + 1) to 20
        if 1 to 10 forall (i =>
          playGame(Seq(a, b, i)) == playGame(Seq(b - a, i))
          )
      } yield {
        a -> b
      }
      println(x.map { case (a, b) => s"case $a :: $b :: other => enumAll(($b-$a)+:other) " }.mkString("\n"))

    }


    "case 3" in {
      println((line2Seq("IIXIIIIIIIXIIIIXIXIIIXIIXIIIIXIIIXIIXXIXXIIIXIIIIXIIIXIIIIIIIXIIXIIIIIIIIIXIIXXIIIIIIIIIIIIIXIXIIIXIIXIXIXIXIIIIXIIIIIXIIXIXIIXIIIIIIIIIXIIIIIIIXIXIIIIXIXIIIIIXXXIIIIIIIIIXIIIIIIIXIIIIXIIIIXIIXI")))
      //      println((line2Seq("IXXIIXXIXIIIIIIIXXIIIIIIIIXIIXIIIIIIIIIIIIIIIIIIIIIIIIIIIIIXXIIXIIIIIIXXIIIXIXIXIIIXIIXIIIIXIXIIIXIXIIIIIXXXIIIIIIIIIIIIIIIIIXIXIXIIIIIIIXIIIXIXIIIIIIXIIIIIXXIIIXIIIXIIIIIIXXIXXXXIIIIIIIIIIIXIIXIXII")))

      //      enumAll(Seq(1, 2, 1, 7, 8, 2, 29, 2, 6, 3, 1, 1, 3, 2, 4, 1, 3, 1, 5, 17, 1, 1, 7, 3, 1, 6, 5, 3, 3, 6, 1, 11, 2, 1, 2))
      playGame(Seq(1, 4, 6, 8, 11, 17, 29))
    }

    "10 to 20" in {
      val l =
        for {
          a <- 10 to 20
          b <- (a + 1) to 20
        } yield ((a, b), playGame(Seq(a, b)))

      l.filter(_._2 == false).foreach(println)
    }
  }
}
