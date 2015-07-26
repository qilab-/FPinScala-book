package chap3

import chap3.datastructures.List

object Exe09 {

  def length[A](as: List[A]): Int = {
    List.foldRight(as, 0)((_, r) => r + 1)
  }

}
