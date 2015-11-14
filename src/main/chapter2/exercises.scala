import scala.annotation.tailrec

object exercises {

  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, previous: Int, current: Int): Int = {
      if (n <= 1)
        previous
      else
        go(n - 1, current, previous + current)
    }
    go(n, 0, 1)
  }

  def isSorted[A](as: Array[A], p: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1)
        true
      else if (p(as(n), as(n + 1)))
        loop (n + 1)
      else
        false
    }
    loop(0)
  }
}
