package laziness

import scala.annotation.tailrec

trait Stream[+A] {

  def headOption(): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // Exercise 5.1
  def toList(): List[A] = {
    @tailrec
    def toList(result: List[A], rest: Stream[A]): List[A] = {
      rest match {
        case Empty => result.reverse
        case Cons(h, t) => toList(h() :: result, t())
      }
    }

    toList(List.empty, this)
  }

  // Exercise 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
    case _ => Stream.empty
  }

  // Exercise 5.2
  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  // Exercise 5.5
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else Stream.empty)

  // Exercise 5.6
  def headOption2(): Option[A] =
    foldRight(Option.empty[A])((a, o) => o.orElse(Some(a)))

  // Exercise 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))

  // Exercise 5.7
  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else t)

  // Exercise 5.7
  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)((h, t) => Stream.cons(h, t))

  // Exercise 5.7
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => f(h) append t)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  // Exercise 5.13
  def map2[B](f: A => B): Stream[B] =
    Stream.unfold(this)(s => s.headOption.map(a => (f(a), s.drop(1))))

  // Exercise 5.13
  def take2(n: Int): Stream[A] = Stream.unfold((this, n)) { case (s, m) =>
    if (m <= 0) None
    else s.headOption.map(a => (a, (s.drop(1), m - 1)))
  }

  // Exercise 5.13
  def takeWhile3(p: A => Boolean): Stream[A] =
    Stream.unfold(this)(s => s.headOption.filter(p).map(a => (a, s.drop(1))))

  // Exercise 5.13
  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = {
    Stream.unfold((this, s)) { case (s1, s2) =>
      for {
        a <- s1.headOption
        b <- s2.headOption
      } yield {
        (f(a, b), (s1.drop(1), s2.drop(1)))
      }
    }
  }

  // Exercise 5.13
  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = {
    Stream.unfold((this, s)) { case (s1, s2) =>
      (s1.headOption, s2.headOption) match {
        case (None, None) => None
        case (o1, o2) => Some(((o1, o2), (s1.drop(1), s2.drop(1))))
      }
    }
  }

  // Exercise 5.14
  def startsWith[B >: A](s: Stream[B]): Boolean = {
    this zipAll s takeWhile(_._2.isDefined) forAll { case (o1, o2) =>
      o1 == o2
    }
//    this zipAll s takeWhile(_._2.isDefined) forAll { case (o1, Some(a2)) =>
//      o1.exists(_ == a2)
//    }
  }

  // Exercise 5.15
  def tails(): Stream[Stream[A]] = {
    Stream.unfold(this) {
      case Empty => None
      case s => Some((s, s.drop(1)))
    } append Stream(Stream.empty)
  }

  // Exercise 5.16
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    this.tails.map(_.foldRight(z)(f))
  }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def ones: Stream[Int] = cons(1, ones)

  // Exercise 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // Exercise 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // Exercise 5.10
  def fibs(a: Int, b: Int): Stream[Int] = cons(a, fibs(b, a + b))
  def fibs(): Stream[Int] = cons(0, fibs(0, 1))

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).fold(empty[A]) { case (a, s) => cons(a, unfold(s)(f)) }

  // Exercise 5.12
  def fibs2(): Stream[Int] = unfold((0, 1)){ case (x, y) => Some((x, (y, x + y))) }

  // Exercise 5.12
  def from2(n: Int): Stream[Int] = unfold(n)(m => Some((m, m + 1)))

  // Exercise 5.12
  def constant2[A](a: A): Stream[A] = unfold(a)(b => Some(b, b))

  // Exercise 5.12
  def ones2: Stream[Int] = constant2(1)

}
