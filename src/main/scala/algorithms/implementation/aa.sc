val left = List(1).map(Option.apply)
val right = List(1, 2).map(Option.apply)
val body = List(List(0), List(9),List(8)).map(Option.apply)
val a: List[((Option[List[Int]], Option[Int]), Option[Int])] =
body zipAll (left, None, None) zipAll (right,(None,None),None )