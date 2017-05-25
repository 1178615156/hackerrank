import scala.collection.Searching._
val l = List(1, 3, 5, 7)

l.inits.filter(_.nonEmpty).map(_.sum).toList.reverse


l.search(1).insertionPoint