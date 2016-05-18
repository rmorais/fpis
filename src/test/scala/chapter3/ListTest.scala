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

  "init" should "return an empty list when called with an empty list" in {
    a [RuntimeException] should be thrownBy{
      List.init(Nil)
      List.init2(Nil)
    }
  }

  it should "return an empty list when called with a single element list" in {
    List.init(singleElemList) should be(Nil)
    List.init2(singleElemList) should be(Nil)
  }

  it should "return the same list without the last element when called with a multiple element list" in {
    List.init(multipleElemList) should be(List(1,2,3,4))
    List.init2(multipleElemList) should be(List(1,2,3,4))
  }

  "length" should "return 0 when the list is empty" in {
    List.length(Nil) should be (0)
  }

  it should "return 1 for a single element list" in {
    List.length(singleElemList) should be (1)
  }

  it should "return the length of a multiple element list" in {
    List.length(multipleElemList) should be (5)
  }

  "sum3" should "return 0 when list is empty" in {
    List.sum3(Nil) should be (0)
  }
  it should "return the sum of all elements of the list" in {
    List.sum3(singleElemList) should be (1)
    List.sum3(multipleElemList) should be (15)
  }

  "product3" should "return 1 when the list is empty" in {
    List.product3(Nil) should be (1.0)
  }

  it should "return the product of all elements of the list" in {
    List.product3(List(2)) should be (2)
    List.product3(List(1,2,3,4,5)) should be (120)
  }

  "length2" should "return 0 when the list is empty" in {
    List.length2(Nil) should be (0)
  }

  it should "return 1 for a single element list" in {
    List.length2(singleElemList) should be (1)
  }

  it should "return the length of a multiple element list" in {
    List.length2(multipleElemList) should be (5)
  }

  "reverse" should "return a list with it's elements in reverse order" in {
    List.reverse(emptyList) should be(Nil)
    List.reverse(singleElemList) should be(singleElemList)
    List.reverse(multipleElemList) should be(List(5,4,3,2,1))
  }

  "appendViaFoldRight" should "return a list containing the two lists appended" in {
    List.appendViaFoldRight(Nil, singleElemList) should be(List(1))
    List.appendViaFoldRight(singleElemList, Nil) should be(List(1))
    List.appendViaFoldRight(multipleElemList, singleElemList) should be(List(1,2,3,4,5,1))
  }

  "appendViaFoldLeft" should "return a list containing the two lists appended" in {
    List.appendViaFoldLeft(Nil, singleElemList) should be(List(1))
    List.appendViaFoldLeft(singleElemList, Nil) should be(List(1))
    List.appendViaFoldLeft(multipleElemList, singleElemList) should be(List(1,2,3,4,5,1))
  }

  "concat" should "return all the lists concatenated into a single list" in {
    List.concat(List(List(1,2), List(3,4))) should be(List(1,2,3,4))
  }

  "add1" should "return all the elements in list incremented by 1" in {
    List.add1(emptyList) should be (Nil)
    List.add1(singleElemList) should be(List(2))
    List.add1(multipleElemList) should be ( List(2,3,4,5,6))
  }
}

