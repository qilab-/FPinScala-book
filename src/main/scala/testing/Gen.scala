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

case class Gen[A](sample: State[RNG, A])

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
}
