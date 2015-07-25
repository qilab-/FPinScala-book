package chap2

object Exe2 {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as.zip(as.tail).forall { case (current, next) =>
      ordered(current, next)
    }
  }

}

