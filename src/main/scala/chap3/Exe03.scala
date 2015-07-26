package chap3

import chap3.datastructures.{Cons, List, Nil}

object Exe03 {

  def setHead[A](h: A, l: List[A]): List[A] = {
    l match {
      case Cons(_, t) => Cons(h, t)
      case Nil => throw new NoSuchElementException
    }
  }

}
