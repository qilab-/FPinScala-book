package chap3

import chap3.datastructures.{List, Nil}
import org.scalatest.{FunSpec, Matchers}

class Exe20Spec extends FunSpec with Matchers {

  describe("flatMap") {
    it("flatMap") {
      Exe20.flatMap(List(1, 2, 3))(i => List(i, i)) should === (List(1, 1, 2, 2, 3, 3))
      Exe20.flatMap(List(1, 2, 3))(i => Nil) should === (Nil)
    }
  }

}
