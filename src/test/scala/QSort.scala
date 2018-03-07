object QSort {
  def swap(array: Array[Int], a: Int, b: Int): Unit = {
    val tmp = array(a)
    array(a) = array(b)
    array(b) = tmp
  }

  def q_sort(array: Array[Int], start: Int, end: Int): Unit = {
    if(end > start) {
      val last = array(end)
      var mid = start
      for (i <- start until end) {
        if(array(i) <= last) {
          swap(array, mid, i)
          mid += 1
        }
      }
      swap(array, mid, end)
      q_sort(array, start, mid - 1)
      q_sort(array, mid + 1, end)
    }
  }

  def main(args: Array[String]): Unit = {


    val array = Array.fill(10)((math.random() * 100).toInt)

    q_sort(array, 0, array.length - 1)
    println(array.toList)
  }
}
