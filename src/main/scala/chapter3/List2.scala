package chapter3

import java.util.NoSuchElementException

/**
  * Created by rmorais on 17/05/2016.
  */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ints: List[Double]): Double = ints match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  //We could also return Nil in the case of getting the tail of an empty list
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => throw new NoSuchElementException
    case Cons(_, xs) => xs
  }

  def setHead[A](head: A, list: List[A]): List[A] = list match {
    case Nil => sys.error("SetHead on empty list")
    case Cons(_, xs) => Cons(head, xs)

  }

}
