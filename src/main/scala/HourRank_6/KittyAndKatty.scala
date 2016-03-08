package HourRank_6

import utils.SetInt

/**
  * Created by yujieshui on 2016/3/3.
  */
object KittyAndKatty {

  def readSeqInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  type User = Boolean

  object Gamer extends Enumeration {
    val kitty = Value("Kitty")
    val katty = Value("Katty")
  }

  import Gamer._

  def play(list: Int): Gamer.Value = {
    val sum = (1 to list).sum
    val lastUser = if (list % 2 == 0) kitty else katty
    sum % 3 match {
      case 1 => kitty
      case 2 => katty
      case 0 => lastUser
    }

  }

  def main(args: Array[String]) {
    val n :: Nil = readSeqInt()
    val data = 1 to n map (_ => readSeqInt().head)
    val out = (data map play)
    println((data,out,right).zipped map {
      case (d,o, r) => (d,o,r,o.toString == r)
    }filter (_._4 == false)
    )
  }

  val right =List(
      "Kitty",
      "Katty",
      "Katty",
      "Katty",
      "Katty",
      "Kitty",
      "Kitty",
      "Katty",
      "Kitty",
      "Kitty",
      "Kitty",
      "Kitty",
      "Katty",
      "Kitty",
      "Kitty",
      "Katty",
      "Katty",
      "Kitty",
      "Katty",
      "Katty",
      "Katty",
      "Katty",
      "Kitty",
      "Kitty",
      "Katty",
      "Katty",
      "Kitty",
      "Katty",
      "Kitty",
      "Katty",
      "Kitty",
      "Katty",
      "Katty",
      "Katty",
      "Kitty",
      "Kitty",
      "Kitty",
      "Katty",
      "Katty",
      "Kitty",
      "Katty",
      "Katty",
      "Katty",
      "Kitty",
      "Kitty",
      "Kitty",
      "Katty",
      "Katty",
      "Katty",
      "Kitty",
      "Kitty",
      "Katty",
      "Kitty",
      "Katty",
      "Kitty",
      "Kitty",
      "Kitty",
      "Katty",
      "Kitty",
      "Kitty",
      "Katty",
      "Katty",
      "Katty",
      "Kitty",
      "Kitty",
      "Katty",
      "Kitty",
      "Kitty",
      "Kitty",
      "Kitty",
      "Katty",
      "Katty",
      "Katty",
      "Katty",
      "Katty",
      "Katty",
      "Katty",
      "Kitty",
      "Kitty",
      "Katty",
      "Kitty",
      "Kitty",
      "Kitty",
      "Kitty",
      "Katty",
      "Katty",
      "Kitty",
      "Katty",
      "Kitty",
      "Katty",
      "Kitty",
      "Katty",
      "Katty",
      "Katty",
      "Katty",
      "Kitty",
      "Katty",
      "Katty",
      "Kitty",
      "Kitty"
  )
//    ".stripMargin.split("\n").toList
  SetInt(
    """100
      |272
      |153
      |933
      |733
      |191
      |10
      |402
      |29
      |922
      |408
      |102
      |366
      |245
      |514
      |882
      |807
      |345
      |704
      |631
      |299
      |517
      |675
      |714
      |114
      |3
      |3
      |964
      |299
      |650
      |489
      |434
      |3
      |369
      |885
      |408
      |732
      |28
      |677
      |407
      |774
      |529
      |157
      |651
      |906
      |110
      |812
      |299
      |595
      |731
      |328
      |382
      |661
      |610
      |789
      |408
      |742
      |218
      |191
      |974
      |678
      |717
      |891
      |339
      |672
      |326
      |29
      |272
      |480
      |180
      |336
      |3
      |489
      |393
      |299
      |967
      |345
      |75
      |838
      |858
      |623
      |258
      |272
      |720
      |844
      |585
      |273
      |604
      |795
      |758
      |299
      |240
      |299
      |897
      |299
      |361
      |696
      |855
      |29
      |66
      |812
    """.stripMargin)
}

/*
100

272   Kitty
153   Katty
933   Katty
733   Katty
191   Katty
10    Kitty
402   Kitty
29    Katty
922   Kitty
408   Kitty
102   Kitty
366   Kitty
245   Katty
514   Kitty
882   Kitty
807   Katty
345   Katty
704   Kitty
631   Katty
299   Katty
517   Katty
675   Katty
714   Kitty
114   Kitty
3   Katty
3   Katty
964   Kitty
299   Katty
650   Kitty
489   Katty
434   Kitty
3   Katty
369   Katty
885   Katty
408   Kitty
732   Kitty
28    Kitty
677   Katty
407   Katty
774   Kitty
529   Katty
157   Katty
651   Katty
906   Kitty
110   Kitty
812   Kitty
299   Katty
595   Katty
731   Katty
328   Kitty
382   Kitty
661   Katty
610   Kitty
789   Katty
408   Kitty
742   Kitty
218   Kitty
191   Katty
974   Kitty
678   Kitty
717   Katty
891   Katty
339   Katty
672   Kitty
326   Kitty
29    Katty
272   Kitty
480   Kitty
180   Kitty
336   Kitty
3   Katty
489   Katty
393   Katty
299   Katty
967   Katty
345   Katty
75    Katty
838   Kitty
858   Kitty
623   Katty
258   Kitty
272   Kitty
720   Kitty
844   Kitty
585   Katty
273   Katty
604   Kitty
795   Katty
758   Kitty
299   Katty
240   Kitty
299   Katty
897   Katty
299   Katty
361   Katty
696   Kitty
855   Katty
29    Katty
66    Kitty
812   Kitty
 */
