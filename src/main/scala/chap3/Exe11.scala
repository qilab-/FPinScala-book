package chap3

import chap3.datastructures.List

object Exe11 {

  def sum(ns: List[Int]): Int = Exe10.foldLeft(ns, 0)(_ + _)

  def product(ds: List[Double]): Double = Exe10.foldLeft(ds, 1.0)(_ * _)

  def length[A](l: List[A]): Int = Exe10.foldLeft(l, 0)((r, _) => r + 1)

}
