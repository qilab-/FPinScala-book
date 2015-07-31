package chap3

import chap3.datastructures.{Branch, Leaf, Tree}

object Exe26 {

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

}
