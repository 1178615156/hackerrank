package mathematics.probability

/**
  * Created by yujieshui on 2017/1/23.
  */
object SherlockAndProbability extends SherlockAndProbability
class SherlockAndProbability {

  def sub(k: Int, size: Int)(array: Array[Boolean]) = {
    val result = 0 until  (size - k) map { i =>
      if(array(i)) {
        val one_size = i to  (i + k) count (e => array.apply(e))
        one_size + one_size * (one_size - 1)
      } else 0
    }
    result.sum
  }

  def solution(array: Array[Boolean], query: Seq[Int]) = {
    val size = array.length
    query.map { k => sub(k, size)(array) }
  }
}
