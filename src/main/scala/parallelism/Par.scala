package parallelism

import java.util.concurrent.TimeUnit

object Par {
  
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // Exercise 7.1
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  // Exercise 7.3
  def map2[A, B, C](timeout: Long, units: TimeUnit)(a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???

  // Exercise 7.4
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  // Exercise 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(lazyUnit(List.empty[A])) { (parA, parList) =>
      map2(parA, parList)(_ :: _)
    }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // Exercise 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val fas: List[Par[A]] = as.map(lazyUnit(_))
    fas.foldRight(lazyUnit(List.empty[A])) { (parA, parList) =>
      fork {
        map2(parA, parList) { (a, l) =>
          val r = if (f(a)) List(a) else List.empty[A]
          r ++ l
        }
      }
    }
  }

}
