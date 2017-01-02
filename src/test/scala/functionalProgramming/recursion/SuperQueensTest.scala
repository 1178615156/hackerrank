package functionalProgramming.recursion

import org.scalatest.{FunSuite, WordSpecLike}

import scala.util.Random

/**
  * Created by yujieshui on 2016/12/31.
  */
class SuperQueensTest extends WordSpecLike {

  import SuperQueens._

  "show " in {
    println(show(mkZone(5)))
  }
  "diagonally" must {

    "leftUp" in {
      println(leftUp((3, 3), 6))
    }
    "rightDown" in {
      println(rightDown((3, 3), 6))
    }


  }

  "mark queen" must {
    "show" in {
      println(show(markQueen(mkZone(3), (1, 1))))
    }
  }
  "mark war" must {
    "show" in {
      markWar(markQueen(mkZone(3), (1, 1)), (1, 1))
        .map(show)
        .foreach(println)
    }
    "show 7" in {
      markWar(markQueen(mkZone(7), (3, 3)), (3, 3))
        .map(show)
        .foreach(println)
    }
  }
  "impl" must {
    "10" in {
      val rt = impl(mkZone(10),0,10)
      println(
        rt.map(show)
          .mkString("\n\n**************\n\n")
      )
      assert(rt.size == 4)
    }
  }

  "batch update " must {
    val size = 20
    val random = new Random(1111)

    "1000 in" in {

      1 to 1000 foreach { _ =>
        val (a, b) = (1 to random.nextInt(size)).foldLeft((mkZone(size), mkZone(size))) {
          case ((a, b), i) =>
            val x = random.nextInt(size)
            val y = random.nextInt(size)
            (
              batchUpdate(attack((x, y), size), Square.attack)(a),
              batchUpdate(attack((x, y), size), Square.attack)(a)
            )
        }
        assert(a === b)
      }

    }
    "eq" in {
      for {
        x <- 0 until 10
        y <- 0 until 10
      } {
        assert(
          batchUpdate(attack((x, y), 10), Square.attack)(mkZone(10)) ===
            batchUpdate(attack((x, y), 10), Square.attack)(mkZone(10))
        )
      }

    }
  }

  /*
(1,1)  :  15.733442 ms
(2,0)  :  1.682126 ms
(3,0)  :  3.13959 ms
(4,0)  :  3.904192 ms
(5,0)  :  8.411383 ms
(6,0)  :  13.040909 ms
(7,0)  :  21.723773 ms
(8,0)  :  23.025296 ms
(9,0)  :  37.48138 ms
(10,4)  :  206.182519 ms
(11,44)  :  315.052097 ms
(12,156)  :  399.617133 ms
(13,1876)  :  2384.681142 ms
(14,5180)  :  14012.559871 ms

   */
  "measurer" must {
    import org.scalameter._

    "total" in {

      1 until 15 foreach { i =>
        println(
          config().withWarmer(Warmer.Zero).measure {
            print(i -> solution(i))
            print("  :  ")
          }
        )
      }
    }

    "12" in {
      println(
        config().withWarmer(Warmer.Zero).measure {
          print(13 -> solution(13))
          print("  :  ")
        }
      )
      println(
        s"""
           |batchUpdate   ${batchUpdateTime}
           |attackTime    ${attackTime}
           |markWarTime   ${markWarTime}
           |findSafeTime  ${findSafeTime}
        """.stripMargin)
    }
    "14" in {
      println(
        config().withWarmer(Warmer.Zero).measure {
          print(14 -> solution(14))
          print("  :  ")
        }
      )
      println(
        s"""
           |batchUpdate   ${batchUpdateTime}
           |attackTime    ${attackTime}
           |markWarTime   ${markWarTime}
           |findSafeTime  ${findSafeTime}
        """.stripMargin)
    }
    "2" in {
      println(solution(10))

    }
  }

}
