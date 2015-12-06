package testing

import laziness.Stream
import state.{RNG, State}
import testing.Prop.Result
import testing.Prop.Types._

// Exercise 8.3
trait Prop2 {
  def check: Boolean

  // Exercise 8.3
  def &&(p: Prop2): Prop2 = new Prop2 {
    override def check: Boolean = {
      if (this.check) p.check else false
    }
  }
}

case class Prop(run: (TestCases, RNG) => Result) {
  // Exercise 8.9
  def &&(p: Prop): Prop = Prop { (testCases, rng) =>
    val result = this.run(testCases, rng)
    if (result.isFalsified)
      result
    else
      p.run(testCases, rng)
  }

  // Exercise 8.9
  def ||(p: Prop): Prop = Prop { (testCases, rng) =>
    val result = this.run(testCases, rng)
    if (result.isFalsified)
      p.run(testCases, rng)
    else
      result
  }
}

object Prop {
  object Types {
    type SuccessCount = Int
    type TestCases = Int
    type FailedCase = String
  }

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    override def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zipWith(Stream.from(0))((_, _)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(r => Some(g.sample.run(r)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
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

  // Exercise 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(if (_) g1 else g2)
  }

  // Exercise 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val p = g1._2 / (g1._2 + g2._2)
    Gen(State(RNG.double)).flatMap { d => if (d < p) g1._1 else g2._1 }
  }
}
