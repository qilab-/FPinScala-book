package chap3

import chap3.datastructures.{Cons, List, Nil}

object Exe23 {

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    (as, bs) match {
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
      case _ => Nil
    }
  }

}
