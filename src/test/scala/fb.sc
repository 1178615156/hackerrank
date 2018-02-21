val l = List(4, 3, 2, 1)
def swap(array: Array[Int], a: Int, b: Int): Array[Int] = {
  val tmp = array(a)
  array(a) = array(b)
  array(b) = tmp
  return array
}
def q_sort(arr: Array[Int], start: Int, end: Int): Unit = {
  if(start >= end) return
  else {
    val last = arr(end - 1)
    var i_start = start
    var i_end = end - 2
    while (i_end >= i_start) {
      if(arr(i_start) < last) {
        i_start += 1
      } else {
        swap(arr, i_start, i_end)
        i_end -= 1
      }
    }
    swap(arr, i_start, end - 1)
    q_sort(arr, start, i_start)
    q_sort(arr, i_start + 1, end)

  }
}
val arr = Array(4,3,5)
q_sort(arr,0,arr.length)
arr