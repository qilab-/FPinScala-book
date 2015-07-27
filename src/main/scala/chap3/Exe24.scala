package chap3

import scala.annotation.tailrec

object Exe24 {

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup match {
      case Nil => false
      case _ if hasSubFirst(sup, sub) => true
      case _ :: tail => hasSubsequence(tail, sub)
    }
  }

  @tailrec
  private[this] def hasSubFirst[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (_, Nil) => true
      case (x :: xs, y :: ys) if (x == y) => hasSubFirst(xs, ys)
      case _ => false
    }
  }

}
