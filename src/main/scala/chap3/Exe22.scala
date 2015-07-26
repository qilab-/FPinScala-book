package chap3

import chap3.datastructures.{Cons, List, Nil}

object Exe22 {

  def zipWithPlus(is1: List[Int], is2: List[Int]): List[Int] = {
    (is1, is2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipWithPlus(t1, t2))
      case _ => Nil
    }
  }

}
