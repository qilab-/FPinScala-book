package datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, tail) => tail
      case Nil => throw new IllegalArgumentException
    }
  }

  // Exercise 3.3
  def setHead[A](h: A, l: List[A]): List[A] = {
    l match {
      case Cons(_, t) => Cons(h, t)
      case Nil => throw new IllegalArgumentException
    }
  }

  // Exercise 3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    (l, n) match {
      case (xs, 0) => xs
      case (Nil, _) => Nil
      case (Cons(_, t), m) => drop(t, m - 1)
    }
  }

  // Exercise 3.5
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, _) if !f(h) => l
      case Cons(h, t) if f(h) => dropWhile(t, f)
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => throw new IllegalArgumentException
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def sum2(ns: List[Int]) = {
    foldRight(ns, 0)((x, y) => x + y)
  }

  def product2(ns: List[Double]) = {
    foldRight(ns, 1.0)(_ * _)
  }

  // Exercise 3.9
  def length[A](as: List[A]): Int = {
    List.foldRight(as, 0)((_, r) => r + 1)
  }

  // Exercise 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  // Exercise 3.11
  def sumByFoldLeft(ns: List[Int]): Int = List.foldLeft(ns, 0)(_ + _)

  // Exercise 3.11
  def productByFoldLeft(ds: List[Double]): Double = List.foldLeft(ds, 1.0)(_ * _)

  // Exercise 3.11
  def lengthByFoldLeft[A](l: List[A]): Int = List.foldLeft(l, 0)((r, _) => r + 1)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] = {
    List.foldLeft(l, Nil: List[A])((r, e) => Cons(e, r))
  }

  // Exercise 3.13
  def foldRightByFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    val rev = List.reverse(as)
    List.foldLeft(rev, z)((a, b) => f(b, a))
  }

  // Exercise 3.14
  def appendByFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    List.foldRight(a1, a2)((a, r) => Cons(a, r))
  }

  // Exercise 3.15
  def flatten[A](l: List[List[A]]): List[A] = {
    List.foldRight(l, Nil: List[A])((e, r) => List.appendByFoldRight(e, r))
  }

  // Exercise 3.16
  def plusOne(l: List[Int]): List[Int] = {
    List.foldRight(l, Nil: List[Int])((e, r) => Cons(e + 1, r))
  }

  // Exercise 3.17
  def toString(l: List[Double]): List[String] = {
    List.foldRight(l, Nil: List[String])((e, r) => Cons(e.toString, r))
  }

  // Exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    List.foldRight(as, Nil: List[B])((e, r) => Cons(f(e), r))
  }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    List.foldRight(as, Nil: List[A]) { (e, r) =>
      if (f(e)) Cons(e, r) else r
    }
  }

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    List.foldRight(as, Nil: List[B])((e, r) => List.append(f(e), r))
  }

  // Exercise 3.21
  def filterByFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    List.flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  // Exercise 3.22
  def zipWithPlus(is1: List[Int], is2: List[Int]): List[Int] = {
    (is1, is2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipWithPlus(t1, t2))
      case _ => Nil
    }
  }

  // Exercise 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    (as, bs) match {
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
      case _ => Nil
    }
  }

}
