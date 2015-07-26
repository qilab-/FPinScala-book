package chap3

import chap3.datastructures.{Cons, List, Nil}

object Exe12 {

  def reverse[A](l: List[A]): List[A] = {
    Exe10.foldLeft(l, Nil: List[A])((r, e) => Cons(e, r))
  }

}
