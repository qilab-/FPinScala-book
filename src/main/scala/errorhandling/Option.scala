package errorhandling

sealed trait Option[+A] {

  // Exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  // Exercise 4.1
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  // Exercise 4.1
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  // Exercise 4.1
  def orElse[B >: A](ob: => Option[B]): Option[B]  = this match {
    case None => ob
    case _ => this
  }

  // Exercise 4.1
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None else Some(xs.sum / xs.length)

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap { m =>
      mean(xs.map(x => math.pow(x - m, 2)))
    }
  }

}
