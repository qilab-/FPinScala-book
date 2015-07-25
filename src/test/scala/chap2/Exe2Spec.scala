package chap2

import org.scalatest.{FunSpec, Matchers}

class Exe2Spec extends FunSpec with Matchers {

  describe("isSorted") {
    it("returns true if the array is sorted by the order") {
      Exe2.isSorted(Array(1, 2, 3, 4, 5), (c: Int, n: Int) => c < n) should equal (true)
      Exe2.isSorted[Int](Array(1, 2, 2, 4, 5), _ < _) should equal (false)
      Exe2.isSorted[Int](Array(1, 2, 3, 4, 4), _ < _) should equal (false)
      Exe2.isSorted[Int](Array(1, 2, 2, 4, 5), _ <= _) should equal (true)
      Exe2.isSorted[Int](Array(1, 2, 1, 4, 5), _ <= _) should equal (false)
      Exe2.isSorted[Int](Array(5, 4, 3, 2, 1), _ > _) should equal (true)

      Exe2.isSorted[String](Array("a", "bb", "ccc"), _.length <= _.length) should equal (true)
      Exe2.isSorted[String](Array("a", "bb", "ccc"), _.length >= _.length) should equal (false)
    }
  }

}

