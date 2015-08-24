package errorhandling

sealed trait Either[+E, +A] {

  // Exercise 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case l: Left[E] => l
    case Right(a) => Right(f(a))
  }

  // Exercise 4.6
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case l: Left[E] => l
    case Right(a) => f(a)
  }

  // Exercise 4.6
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case l: Left[E] => b
    case r: Right[A] => r
  }

  // Exercise 4.6
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    flatMap { a =>
      b.map { bx =>
        f(a, bx)
      }
    }
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  // Exercise 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es.foldRight[Either[E, List[A]]](Right(Nil)) { (e, el) =>
      el.flatMap { l =>
        e.map(_ :: l)
      }
    }
  }

  // Exercise 4.7
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldRight[Either[E, List[B]]](Right(Nil)) { (a, el) =>
      el.flatMap { l =>
        f(a).map(_ :: l)
      }
    }
  }

}
