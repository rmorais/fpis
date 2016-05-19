package chapter4

/**
  * Created by rmorais on 19/05/2016.
  */
trait Option[+A] {

  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(v) => Some(f(v))
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(v) => v
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def flatMap2[B](f: A => Option[B]): Option[B] = {
    this match {
      case None => None
      case Some(v) => f(v)
    }
  }

  def orElse[B >: A](default: => Option[B]): Option[B] = this map (Some(_)) getOrElse default

  def orElse2[B >: A](default: => Option[B]): Option[B] = {
    this match {
      case None => default
      case o => o
    }
  }

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

  def filter2(f: A => Boolean): Option[A] = {
    this match {
      case Some(v) if f(v) => Some(v)
      case _ => None
    }
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap {
      m =>
        mean(xs map (x => math.pow(x - m, 2)))
    }
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
    a flatMap (a1 => b map(b1 => f(a1,b1)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => None
      case (maybeX :: xs) => maybeX flatMap(x => sequence(xs) map (x :: _))
    }
  }

  /*
  It can also be implemented using `foldRight` and `map2`. The type annotation on `foldRight` is needed here; otherwise
  Scala wrongly infers the result type of the fold as `Some[Nil.type]` and reports a type error (try it!). This is an
  unfortunate consequence of Scala using subtyping to encode algebraic data types.
  */
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

  //Exercise 4.5
  //naive implementation that looks at list twice
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    sequence (a map f)
  }

  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case (x :: xs) => map2(f(x), traverse(xs)(f))(_ :: _)
    }
  }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

}
