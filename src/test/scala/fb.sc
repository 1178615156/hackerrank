def a(i: Int) = i + 1
val l = List(1,2,3,1)
l.map(a)
l.map(a _)
l.map(a(_))


val f1 = a(_)
val f2 = a _


l.span(_ <=2)