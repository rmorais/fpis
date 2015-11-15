/**
  * Created by rmorais on 15/11/2015.
  */

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else
      Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil // We could also generate an error like: case Nil => sys.error("Tail of an empty list")
    case Cons(_, xs) => xs
  }

  def setHead[A](head: A, l: List[A]): List[A] = l match {
    case Nil => sys.error("Cannot set the head of an empty list")
    case Cons(_, xs) => Cons(head, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else
      l match {
        case Nil => Nil
        case Cons(_, tail) => drop(tail, n-1)
      }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  def dropWhile2[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case l => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Init of an empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }
}