package chapter5

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by rmorais on 16/06/2016.
  */
class StreamTest extends FlatSpec with Matchers {

  "take" should "return empty stream if n is zero or less" in {
    Stream(1 to 10 : _*).take(0) should be(Empty)
    Stream(1 to 10 : _*).take(-5) should be(Empty)
  }
  it should "return a stream with the first n elements" in {
    Stream(1 to 1000 : _*).take(1).toList should be(Stream(1).toList)
    Stream(1 to 1000 : _*).take(53).toList should be(Stream(1 to 53: _*).toList)
  }

  "drop" should "return the stream if n is zero or less" in {
    Stream(1 to 10 : _*).drop(0).toList should be(Stream(1 to 10: _*).toList)
    Stream(1 to 10 : _*).drop(-10).toList should be(Stream(1 to 10: _*).toList)
  }

  it should "return the stream without the first n elements" in {
    var n = 3
    Stream(1 to 10 : _*).drop(n).toList should be(Stream(n + 1 to 10: _*).toList)
    n = 1
    Stream(1 to 10 : _*).drop(n).toList should be(Stream(n + 1 to 10: _*).toList)
  }

  it should "return an empty stream if n is bigger than the stream length" in {
    Stream(1 to 10 : _*).drop(20) should be(Empty)
  }

  "takeWhile" should "return empty stream if predicate is false" in {
    Stream(1 to 1000 : _*).takeWhile(_ => false) should be(Empty)
  }

  it should "return the first elements that verify the predicate" in {
    Stream(1 to 1000 : _*).takeWhile(_ < 10).toList should be(Stream(1 until 10: _*).toList)
  }

}
