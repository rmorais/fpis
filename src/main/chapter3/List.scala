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
    case Cons(_,xs) => xs
  }

  def setHead[A](head: A, l: List[A]): List[A] = l match {
    case Nil => sys.error("Cannot set the head of an empty list")
    case Cons(_, xs) => Cons(head, xs)
  }
}