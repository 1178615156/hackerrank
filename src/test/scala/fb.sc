def isNumber(s:Char) = 1 to 9 map(_.toString.head) contains s

"111 + 222" takeWhile isNumber