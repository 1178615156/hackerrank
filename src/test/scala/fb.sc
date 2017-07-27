val l = List(1,2,3,4,5)

val ll = l.inits.toList.tail.reverse  zip l.tails.toList

ll.map{case (l,r)⇒(r.head,l,r.tail)}