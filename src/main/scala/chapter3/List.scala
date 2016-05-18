package chapter3

import java.util.NoSuchElementException

import scala.annotation.tailrec

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
  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = {
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

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

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
        case Cons(_, xs) => drop(xs, n - 1)
      }

  //Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
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
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
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
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  //Exercise 3.10
  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  //Exercise 3.11
  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  //Exercise 3.12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))

  //Exercise 3.13
  //Need to come back to this as I haven't fully understood the implementation
  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((acc, h) => f(h, acc))
  }

  def foldRightViaFoldLeft2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, (b: B) => b)((g,a)=> b => g(f(a,b)))(z)
  }

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
  }

  //Exercise 3.14
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((acc, h) => Cons(h, acc))

  //Exercise 3.15
  /*
  Since `append` takes time proportional to its first argument, and this first argument never grows because of the
  right-associativity of `foldRight`, this function is linear in the total length of all lists. You may want to try
  tracing the execution of the implementation on paper to convince yourself that this works.
  Note that we're simply referencing the `append` function, without writing something like `(x,y) => append(x,y)`
  or `append(_,_)`. In Scala there is a rather arbitrary distinction between functions defined as _methods_, which are
  introduced with the `def` keyword, and function values, which are the first-class objects we can pass to other
  functions, put in collections, and so on. This is a case where Scala lets us pretend the distinction doesn't exist.
  In other cases, you'll be forced to write `append _` (to convert a `def` to a function value)
  or even `(x: List[A], y: List[A]) => append(x,y)` if the function is polymorphic and the type arguments aren't known.
  */
  def concat[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(append)

  //Exercise 3.16
  def add1(l: List[Int]): List[Int] = foldRight(l, List[Int]())((h, acc) => Cons(h + 1, acc))

  //Exercise 3.17
  def doubleToString(l: List[Double]): List[String] = foldRight(l, List[String]())((h, acc) => Cons(h.toString, acc))

  //Exercise 3.18
  /*
  A natural solution is using `foldRight`, but our implementation of `foldRight` is not stack-safe. We can
  use `foldRightViaFoldLeft` to avoid the stack overflow (variation 1), but more commonly, with our current
  implementation of `List`, `map` will just be implemented using local mutation (variation 2). Again, note that the
  mutation isn't observable outside the function, since we're only mutating a buffer that we've allocated.
  */
  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def map2[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((h, acc) => Cons(f(h), acc))

  def map3[A,B](l: List[A])(f: A => B): List[B] = foldRightViaFoldLeft(l, List[B]())((h, acc) => Cons(f(h), acc))

  def map4[A,B](l: List[A])(f: A => B): List[B] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[B]
    @tailrec
    def go(l: List[A]): List[B] = l match {
      case Nil => Nil
      case Cons(x, xs) => buf += f(x); go(xs)
    }
    go(l)
    List(buf: _*)
  }

  //Exercise 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @tailrec
    def go(l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) buf += x; go(xs)
    }
    go(l)
    List(buf: _*)
  }

  //Exercise 3.20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = foldLeft(l, List[B]())((acc, h) => append(acc, f(h)))

  def flatMap2[A,B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))
}
