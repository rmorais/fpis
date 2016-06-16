package chapter5

/**
  * Created by rmorais on 16/06/2016.
  */

import scala.{Stream => _}
import Stream._

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Option(h())
  }

  //Not tail recursive so it might give a stack overflow for large streams
  def toListNaive: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toList: List[A] = {
    @scala.annotation.tailrec
    def loop(stream: Stream[A], acc: List[A]): List[A] = {
      stream match {
        case Empty => acc
        case Cons(h, t) => loop(t(), h() :: acc)
      }
    }

    loop(this, Nil).reverse
  }

/*
In order to avoid the `reverse` at the end, we could write it using a
mutable list buffer and an explicit loop instead. Note that the mutable
list buffer never escapes our `toList` method, so this function is
still _pure_.
*/
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), Empty)
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => Empty
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](hd: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
