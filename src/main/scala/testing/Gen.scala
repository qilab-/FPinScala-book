package testing

import state.{RNG, State}

trait Prop {
  def check: Boolean

  // Exercise 8.3
  def &&(p: Prop): Prop = new Prop {
    override def check: Boolean = {
      if (this.check) p.check else false
    }
  }
}

case class Gen[A](sample: State[RNG, A]) {
  // Exercise 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(
    sample.flatMap { a =>
      f(a).sample
    }
  )

  // Exercise 8.6
  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => Gen.listOfN(n, this))
  }
}

object Gen {
  // Exercise 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val (s, e) =
      if (start < stopExclusive)
        (start, stopExclusive)
      else
        (stopExclusive, start)
    val run: RNG => (Int, RNG) = {
      val rand = RNG.nonNegativeLessThan(e - s)
      RNG.map(rand)(_ + s)
    }
    val state = State[RNG, Int](run)
    Gen(state)
  }

  // Exercise 8.5
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  // Exercise 8.5
  def boolean: Gen[Boolean] = Gen(
    choose(0, 2).sample.map(_ == 0)
  )
  def boolean_2: Gen[Boolean] = Gen(
    State[RNG, Boolean](
      RNG.map(RNG.nonNegativeLessThan(2))(_ == 0)
    )
  )

  // Exercise 8.5
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(
    State.sequence(List.fill(n)(g.sample))
  )
}
