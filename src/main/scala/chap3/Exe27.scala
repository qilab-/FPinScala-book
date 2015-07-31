package chap3

import chap3.datastructures.{Branch, Leaf, Tree}

object Exe27 {

  def depth(t: Tree[_]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

}
