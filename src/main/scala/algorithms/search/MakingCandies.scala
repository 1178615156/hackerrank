package algorithms.search

object MakingCandies {

  case class Entity(machine: Int, worker: Int, price: Int, candies: Int) {
    def makeCandies = machine * worker

    def makeIsFavorable = machine >= price || worker >= price

    def make = this.copy(candies = candies + makeCandies)

    def canBuyNum = candies / price
  }

  def buyOne(entity: Entity)={

  }
  def buyAll(entity: Entity): Entity = {
    import entity._
    val canBuyHalfNum = canBuyNum / 2
    val canBuyLeftNum = canBuyNum - canBuyHalfNum

    if(worker > machine)
      buyAll(entity.copy(worker = machine, machine = worker))
    else if(machine == worker) {
      Entity(
        machine = machine + canBuyHalfNum,
        worker = machine + canBuyLeftNum,
        price = price,
        candies = candies % price)
    } else {
      val diff = math.abs(machine - worker)
      if(canBuyNum <= diff)
        entity.copy(worker = worker + diff, candies = candies - diff * price)
      else {
        buyAll(entity.copy(worker = worker + diff, candies = candies - diff * price))
      }
    }
  }

  def solution(entity: Entity, goal: Long, iterNum: Int): Int = {
//    if(entity.candies >= goal)
//      iterNum
//    else {
//      val newEntity = entity.make
//      if(newEntity.makeIsFavorable)
//        solution(buyAll(newEntity), goal, iterNum + 1)
//      else {
//
//      }
//
//    }
    ???

  }
}
