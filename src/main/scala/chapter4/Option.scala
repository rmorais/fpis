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

}
