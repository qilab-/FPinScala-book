package chap3

import chap3.datastructures.{Cons, List, Nil}
import scala.annotation.tailrec

object Exe10 {

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

}
