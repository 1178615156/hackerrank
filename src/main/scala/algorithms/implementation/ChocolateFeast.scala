package algorithms.implementation

import utils.SetInt

/**
  * Created by yuJieShui on 2016/3/5.
  */
object ChocolateFeast {

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  case class Data(pocketMoney: Int, chocolateMoney: Int, needWrapperNum: Int)

  def finalEatChocolateNum(data: Data) = {

    val moneyBuyChocolateNum = data.pocketMoney / data.chocolateMoney
    def wrapperBuyNum(wrapperNum: Int, rt: Int = 0): Int = {
      if (wrapperNum < data.needWrapperNum)
        rt
      else
        wrapperBuyNum(
          wrapperNum % data.needWrapperNum + wrapperNum / data.needWrapperNum,
          wrapperNum / data.needWrapperNum + rt
        )
    }

    moneyBuyChocolateNum + wrapperBuyNum(moneyBuyChocolateNum)


  }

  def main(args: Array[String]) {
    val n :: Nil = readListInt()
    val data = 1 to n map (_ => {
      val pocketMoney :: chocolateMoney :: wrapperNum :: Nil = readListInt()
      Data(pocketMoney, chocolateMoney, wrapperNum)
    })
    val result = data map finalEatChocolateNum
    val out = result mkString "\n"
    println(out)
  }


  SetInt(
    """3
      |10 2 5
      |12 4 4
      |6 2 2
    """.stripMargin)
}
