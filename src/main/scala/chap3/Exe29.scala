package chap3

import chap3.datastructures.{Branch, Leaf, Tree}

object Exe29 {

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  def size(t: Tree[_]): Int = {
    fold(t)(_ => 1)(_ + _ + 1)
  }

  def maximun(t: Tree[Int]): Int = {
    fold(t)(a => a)(_ max _)
  }

  def depth(t: Tree[_]): Int = {
    fold(t)(_ => 1)(_.max(_) + 1)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold[A, Tree[B]](t)(a => Leaf(f(a)))((l, r) => Branch(l, r))
  }

}
