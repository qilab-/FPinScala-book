package chap2

import org.scalatest.{FunSpec, Matchers}

class Exe1Spec extends FunSpec with Matchers {

  val fibs = Seq(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946)

  describe("fib") {
    it("returns nth fib number") {
      fibs.zipWithIndex.foreach { case (f, i) =>
        Exe1.fib(i + 1) should === (f)
      }
    }
  }

}

