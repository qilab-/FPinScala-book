package chap3

import chap3.datastructures.{List, Nil}

object Exe20 {

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    Exe13.foldRight(as, Nil: List[B])((e, r) => Exe14.append(f(e), r))
  }

}
