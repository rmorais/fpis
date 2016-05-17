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

  "setHead" should "throw RuntimeException if called on an empty list" in {
    a [RuntimeException] should be thrownBy{
      List.setHead(1, emptyList)
    }
  }

  it should "replace the head of a non empty list" in {
    List.setHead(6, singleElemList) should be (List(6))
    List.setHead(6, multipleElemList) should be (List(6,2,3,4,5))
  }

  "drop" should "return an empty list if called on an empty list" in {
    List.drop(emptyList, 3) should be (Nil)
  }

  it should "return an empty list if called with a single element list" in {
    List.drop(singleElemList, 1) should be (Nil)
  }

  it should "return a list with the specified elements dropped" in {
    List.drop(multipleElemList, 4) should be (List(5))
  }

  it should "return an empty list when requested to drop more elements than the list length" in {
    List.drop(multipleElemList, 6) should be (Nil)
  }

  it should "return the same list when requested to drop less than one element" in {
    List.drop(multipleElemList, 0) should be (multipleElemList)
    List.drop(multipleElemList, -1) should be (multipleElemList)
  }

  "dropWhile" should "return an empty list if called on an empty list" in {
    List.dropWhile(emptyList, (x: Int) =>  x < 2) should be (Nil)
  }

  it should "drop elements while the condition is true" in {
    List.dropWhile(singleElemList, (x: Int) => x < 2) should be (Nil)
    List.dropWhile(multipleElemList, (x: Int) => x < 2) should be (List(2,3,4,5))
    List.dropWhile(multipleElemList, (x: Int) => true) should be (Nil)
  }

}

