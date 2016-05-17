package chapter3

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by rmorais on 17/05/2016.
  */
class ListTest extends FlatSpec with Matchers {

  behavior of "ListTest"

  val emptyList = Nil
  val singleElemList = Cons(1, Nil)
  val multipleElemList = List(1,2,3,4,5)

  it should "throw NoSuchElementException if tail of an empty list" in {
    a [NoSuchElementException] should be thrownBy{
      List.tail(emptyList)
    }
  }

  it should "return Nil if tail on a single element list" in {
    List.tail(singleElemList) should be (Nil)
  }

  it should "return the tail if tail on a multiple element list" in {
    List.tail(multipleElemList) should be (List(2,3,4,5))
  }

}

