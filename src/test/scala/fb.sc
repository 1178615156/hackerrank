val a=List(1,2,3,4,5).inits.toList.tail.reverse
val b =List(1,2,3,4,5).tails.toList.tail
a zip b map {case(a,b) => a ++ b }
Nil