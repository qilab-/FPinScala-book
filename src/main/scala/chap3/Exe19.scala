package chap3

import chap3.datastructures.{Cons, List, Nil}

object Exe19 {

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    Exe13.foldRight(as, Nil: List[A]) { (e, r) =>
      if (f(e)) Cons(e, r) else r
    }
  }

}
