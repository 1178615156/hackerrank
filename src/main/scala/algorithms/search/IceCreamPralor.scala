//package algorithms.search
//
///**
//  * Created by yuJieShui on 2016/5/14.
//  */
//object IceCreamPralor {
//
//  case class Ice(index: Int, value: Int)
//
//  def clhoose(amount: Int, product: Seq[Int]) = {
//    val ices =
//      (1 to product.size zip product map { case (i, v) => Ice(i, v) }).sortBy(_.value)
//
//    ices.map(first =>{
//
//    })
//    def chooseImpl(first: Ice, last: Ice, list: Seq[Ice]): Some[(Ice, Ice)] = {
//      first.value + last.value match {
//        case x if x == amount => Some(first -> last)
//        case x if x > amount =>
//      }
//
//    }
//  }
//}
