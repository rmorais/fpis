package chapter3

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by rmorais on 17/05/2016.
  */
class ListTest extends FlatSpec with Matchers {

  val emptyList: List[Int] = Nil
  val singleElemList = Cons(1, Nil)
  val multipleElemList: List[Int] = List(1,2,3,4,5)

  "tail" should "throw NoSuchElementException if called with an empty list" in {
    a [NoSuchElementException] should be thrownBy{
      List.tail(emptyList)
    }
  }

  it should "return Nil if called with a single element list" in {
    List.tail(singleElemList) should be (Nil)
  }

  it should "return the tail of the list if called with a multiple element list" in {
    List.tail(multipleElemList) should be (List(2,3,4,5))
  }

  "setHead" should "throw NoSuchElementException if called on an empty list" in {
    a [RuntimeException] should be thrownBy{
      List.setHead(1, emptyList)
    }
  }

  it should "replace the head of a non empty list" in {
    List.setHead(6, singleElemList) should be (List(6))
    List.setHead(6, multipleElemList) should be (List(6,2,3,4,5))
  }
}

