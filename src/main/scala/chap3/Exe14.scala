package chap3

import chap3.datastructures.{Cons, List}

object Exe14 {

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    Exe13.foldRight(a1, a2)((a, r) => Cons(a, r))
  }

}
