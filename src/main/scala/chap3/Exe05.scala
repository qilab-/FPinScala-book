package chap3

import chap3.datastructures.{Cons, List, Nil}
import scala.annotation.tailrec

object Exe05 {

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, _) if !f(h) => l
      case Cons(h, t) if f(h) => dropWhile(t, f)
    }
  }

}
