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

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(x, xs) => Cons(x, append(xs, a2))
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

  def drop[A](l: List[A], n: Int): List[A] =
    if (n < 1) l
      else
    l match {
    case Nil => Nil
    case Cons(_, xs) => drop(xs, n-1)
  }

  def dropWhile[A](l: List[A], f : A => Boolean): List[A] = {
      l match {
        case Nil => Nil
        case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
      }
  }


  //This method uses a stack frame for each list element, which might led to a stackoverflow if the list is big enough
  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException("Init on an empty list")
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }
}
