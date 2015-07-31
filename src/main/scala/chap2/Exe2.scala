package chap2

object Exe2 {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as.zip(as.tail).forall { case (current, next) =>
      ordered(current, next)
    }
  }

  def isSorted2[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as.zip(as.tail).forall(ordered.tupled)
  }

  def isSortedRec[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int, acc: Boolean): Boolean = {
      if (n <= 0)
        acc
      else
        loop(n - 1, acc && ordered(as(n - 1), as(n)))
    }

    loop(as.length - 1, true)
  }

}

