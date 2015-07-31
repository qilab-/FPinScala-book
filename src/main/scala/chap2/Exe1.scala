package chap2

import scala.annotation.tailrec

object Exe1 {

  def fib(n: Int): Int = {
    @tailrec
    def fibRec(n: Int, prev: Int = 0, result: Int = 1): Int = {
      if (n <= 1)
        0
      else if (n == 2)
        result
      else
        fibRec(n - 1, result, prev + result)
    }

    if (n < 1)
      throw new NoSuchElementException
    else
      fibRec(n)
  }

}

