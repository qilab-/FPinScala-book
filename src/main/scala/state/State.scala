package state

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  // Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    val n = if (i == Int.MinValue) 0 else Math.abs(i)
    (n, r)
  }

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (n, r) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), r)
  }

  // Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  // Exercise 6.3
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  // Exercise 6.3
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @scala.annotation.tailrec
    def ints(c: Int, r: RNG, l: List[Int]): (List[Int], RNG) = {
      if (c <= 0)
        (l.reverse, r)
      else {
        val (ii, rr) = r.nextInt
        ints(c - 1, rr, ii :: l)
      }
    }

    ints(count, rng, List.empty[Int])
  }

  // Exercise 6.4
  def intsS(count: Int)(rng: RNG): (List[Int], RNG) = {
    Stream.iterate((List.empty[Int], rng)) { case (is, r) =>
      val (ii, rr) = r.nextInt
      (ii :: is, rr)
    }.take(count + 1).last
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // Exercise 6.5
  def double2(rng: RNG): (Double, RNG) =
    map(nonNegativeInt) { _ / (Int.MaxValue.toDouble + 1) }(rng)

  // Exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]](r => (List.empty[A], r)) { map2(_, _)(_ :: _) }
    // fs.foldRight(unit(List.empty[A])) { (f, acc) => map2(f, acc)(_ :: _) }

  // Exercise 6.7
  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => fs.foldRight((List.empty[A], rng)) { case (rand, (l, r)) =>
      val (a, rr) = rand(r)
      (a :: l, rr)
    }

  // Exercise 6.7
  def ints2(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence { List.fill[Rand[Int]](count)(int) }(rng)

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    // check is always positive mathematically but if the RHS > Int.MaxValue then check becomes negative because of overflow
    val check = i + (n - 1) - mod
    if (check >= 0)
      (mod, rng2)
    else
      nonNegativeLessThan(n)(rng2)
  }

  // Exercise 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  // Exercise 6.8
  def nonNegativeLessThan2(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
//        rng => (mod, rng)
        unit(mod)
      else
        nonNegativeLessThan2(n)
    }

  // Exercise 6.9
  def map_2[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  // Exercise 6.9
  def map2_2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(both(ra, rb)) { case (a, b) => unit(f(a, b)) }

}

case class State[S, +A](run: S => (A, S)) {

  // Exercise 6.10
  def unit: State[S, A] = this

  // Exercise 6.10
  def map[B](f: A => B): State[S, B] = {
    val run2: S => (B, S) = s => {
      val (a, s2) = run(s)
      (f(a), s2)
    }
    State(run2)
  }

  // Exercise 6.10
  def map2[B, C](other: State[S, B])(f: (A, B) => C): State[S, C] = {
    val run2: S => (C, S) = s => {
      val (a, s2) = run(s)
      val (b, s3) = other.run(s2)
      (f(a, b), s3)
    }
    State(run2)
  }

  // Exercise 6.10
  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    val run2: S => (B, S) = s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    }
    State(run2)
  }


}

object State {
  type Rand[A] = State[RNG, A]

  // Exercise 6.10
  def sequence[S, A](list: List[State[S, A]]): State[S, List[A]] = {
    val run2: S => (List[A], S) =
      s => list.foldRight((List.empty[A], s)) { case (state, (l, ss)) =>
        val (a, s2) = state.run(ss)
        (a :: l, s2)
      }
    State(run2)
  }

}
