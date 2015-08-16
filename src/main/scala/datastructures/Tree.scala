package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 3.25
  def size(t: Tree[_]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  // Exercise 3.26
  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  // Exercise 3.27
  def depth(t: Tree[_]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  // Exercise 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  // Exercise 3.29
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  // Exercise 3.29
  def sizeByFold(t: Tree[_]): Int = {
    fold(t)(_ => 1)(_ + _ + 1)
  }

  // Exercise 3.29
  def maximumByFold(t: Tree[Int]): Int = {
    fold(t)(a => a)(_ max _)
  }

  // Exercise 3.29
  def depthByFold(t: Tree[_]): Int = {
    fold(t)(_ => 1)(_.max(_) + 1)
  }

  // Exercise 3.29
  def mapByFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold[A, Tree[B]](t)(a => Leaf(f(a)))((l, r) => Branch(l, r))
  }

}
