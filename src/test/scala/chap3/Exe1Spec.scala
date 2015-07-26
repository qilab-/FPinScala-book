package chap3

import chap3.datastructures.{Cons, List, Nil}
import org.scalatest.{FunSpec, Matchers}

class Exe1Spec extends FunSpec with Matchers {

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  it("x should equals to 3") {
    x should === (3)
  }

}
