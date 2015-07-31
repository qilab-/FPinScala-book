package chap3

import chap3.datastructures.{Branch, Leaf, Tree}

object Exe25 {

  def size(t: Tree[_]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

}
