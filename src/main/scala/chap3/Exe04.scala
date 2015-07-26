package chap3

import chap3.datastructures.{Cons, List, Nil}
import scala.annotation.tailrec

object Exe04 {

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    (l, n) match {
      case (xs, 0) => xs
      case (Nil, _) => Nil
      case (Cons(_, t), m) => drop(t, m - 1)
    }
  }

}
