package chap3

import chap3.datastructures.{Cons, List, Nil}

object Exe02 {

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, tail) => tail
      case Nil => throw new NoSuchElementException
    }
  }

}
