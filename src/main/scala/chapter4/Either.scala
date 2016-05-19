package chapter4

/**
  * Created by rmorais on 19/05/2016.
  */

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, Right => _, Left => _}

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }
  }

  def flatMap[EE >:E,B](f: A => Either[EE,B]): Either[EE, B] = {
    this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }
  }

  def orElse[EE >:E,B >: A](b: Either[EE,B]): Either[EE, B] = {
    this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }
  }

  def map2[EE >:E,B,C](b: Either[EE,B])(f: (A,B) => C): Either[EE, C] = {
    this flatMap(aa => b map(bb => f(aa, bb)))
  }

  def mapViaFor[EE >:E,B,C](b: Either[EE,B])(f: (A,B) => C): Either[EE, C] = {
    for {
      aa <- this
      bb <- b
    } yield f(aa,bb)
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
