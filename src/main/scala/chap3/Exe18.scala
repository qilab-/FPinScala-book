package chap3

import chap3.datastructures.{Cons, List, Nil}

object Exe18 {

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    Exe13.foldRight(as, Nil: List[B])((e, r) => Cons(f(e), r))
  }

}
