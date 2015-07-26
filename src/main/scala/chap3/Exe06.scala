package chap3

import chap3.datastructures.{Cons, List, Nil}

object Exe06 {

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil | Cons(_, Nil) => throw new NoSuchElementException
      case Cons(h, Cons(_, Nil)) => Cons(h, Nil)
      case Cons(h, t) => Cons(h, init(t))
    }
  }

}
