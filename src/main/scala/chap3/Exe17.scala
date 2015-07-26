package chap3

import chap3.datastructures.{Cons, List, Nil}

object Exe17 {

  def toString(l: List[Double]): List[String] = {
    Exe13.foldRight(l, Nil: List[String])((e, r) => Cons(e.toString, r))
  }

}
