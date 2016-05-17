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

  //alternative implementation
  def dropWhile2[A](l: List[A])(f : A => Boolean): List[A] = {
    l match {
      case Cons(x, xs) if f(x) => dropWhile2(xs)(f)
      case _ => l
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  //Exercise 3.2
  //We could also return Nil in the case of getting the tail of an empty list
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => throw new NoSuchElementException
    case Cons(_, xs) => xs
  }

  //Exercise 3.3
  def setHead[A](head: A, list: List[A]): List[A] = list match {
    case Nil => sys.error("SetHead on empty list")
    case Cons(_, xs) => Cons(head, xs)

  }

  //Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n < 1) l
      else
    l match {
    case Nil => Nil
    case Cons(_, xs) => drop(xs, n-1)
  }

  //Exercise 3.5
  def dropWhile[A](l: List[A], f : A => Boolean): List[A] = {
      l match {
        case Nil => Nil
        case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
      }
  }

  //Exercise 3.6
  //This method uses a stack frame for each list element, which might led to a stackoverflow if the list is big enough
  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException("Init on an empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }

  /*
  Note that we're copying the entire list up until the last element. Besides being inefficient, the natural recursive
  solution will use a stack frame for each element of the list, which can lead to stack overflows for
  large lists (can you see why?). With lists, it's common to use a temporary, mutable buffer internal to the
  function (with lazy lists or streams, which we discuss in chapter 5, we don't normally do this). So long as the
  buffer is allocated internal to the function, the mutation is not observable and RT is preserved.
  Another common convention is to accumulate the output list in reverse order, then reverse it at the end, which
  doesn't require even local mutation. We'll write a reverse function later in this chapter.
  */


  //Exercise 3.7
  /*
  No, this is not possible! The reason is because _before_ we ever call our function, `f`, we evaluate its argument,
  which in the case of `foldRight` means traversing the list all the way to the end. We need _non-strict_ evaluation
  to support early termination---we discuss this in chapter 5.
  */

  //Exercise 3.8
  /*
  we get back the original list
  */

  //Exercise 3.9
  def length[A](l: List[A]): Int = foldRight(l,0)((_,acc) => acc + 1)
}
