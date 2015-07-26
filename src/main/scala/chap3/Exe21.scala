package chap3

import chap3.datastructures.{List, Nil}

object Exe21 {

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    Exe20.flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

}
