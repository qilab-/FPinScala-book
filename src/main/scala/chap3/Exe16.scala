package chap3

import chap3.datastructures.{Cons, List, Nil}

object Exe16 {

  def plusOne(l: List[Int]): List[Int] = {
    Exe13.foldRight(l, Nil: List[Int])((e, r) => Cons(e + 1, r))
  }

}
