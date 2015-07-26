package chap3

import chap3.datastructures.{List, Nil}

object Exe15 {

  def flatten[A](l: List[List[A]]): List[A] = {
    Exe13.foldRight(l, Nil: List[A])((e, r) => Exe14.append(e, r))
  }

}
