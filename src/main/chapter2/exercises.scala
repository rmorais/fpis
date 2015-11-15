import scala.annotation.tailrec

object exercises {

  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, previous: Int, current: Int): Int = {
      if (n == 0)
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

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    b => f(a, b)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a,b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
