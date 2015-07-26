package chap3

import chap3.datastructures.List

object Exe13 {

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    // Exe12.reverse() method uses Exe10.foldLeft
    val rev = Exe12.reverse(as)
    Exe10.foldLeft(rev, z)((a, b) => f(b, a))
  }

}
