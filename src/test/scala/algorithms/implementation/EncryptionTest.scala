package algorithms.implementation

import org.scalatest.FunSuite

/**
  * Created by yuJieShui on 2016/5/7.
  */
class EncryptionTest extends FunSuite {

  import Encryption._

  test("encryption 1") {
    assert(encryption("haveaniceday") == "hae and via ecy")
  }
  test("encryption 2") {
    assert(encryption("feedthedog") == "fto ehg ee dd")
  }
  test("encryption 3") {
    assert(encryption("chillout") == "clu hlt io")
  }
}
