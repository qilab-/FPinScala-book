package chap2

import scala.annotation.tailrec

object Exe1 {

  def fib(n: Int): Int = {
    @tailrec
    def fibRec(n: Int, prev: Int = 0, result: Int = 1): Int = {
      n match {
        case 1 => 0
	case 2 => result
	case _ => fibRec(n - 1, result, prev + result)
      }
    }

    fibRec(n)
  }

}

