package chap3

import org.scalatest.{FunSpec, Matchers}

class Exe24Spec extends FunSpec with Matchers {

  describe("hasSubsequence") {
    it("returns true if sup has subsequence of sub") {
      val sup = List(1, 2, 3, 4)
      Exe24.hasSubsequence(sup, List(1, 2)) should === (true)
      Exe24.hasSubsequence(sup, List(2, 3)) should === (true)
      Exe24.hasSubsequence(sup, List(4)) should === (true)
      Exe24.hasSubsequence(sup, List(2, 3, 4)) should === (true)
      Exe24.hasSubsequence(sup, List(1, 2, 3, 4)) should === (true)

      Exe24.hasSubsequence(sup, List(1, 3)) should === (false)
      Exe24.hasSubsequence(sup, List(2, 4)) should === (false)
    }
  }

}
