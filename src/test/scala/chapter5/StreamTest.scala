package chapter5

import org.scalatest.{Matchers, FlatSpec}

import scala.collection.immutable.Stream.cons

/**
 * Created by nicole on 7/13/16.
 */
class StreamTest extends FlatSpec with Matchers {

  val intStream = Stream(1, 2, 3, 4, 5)

  "Stream" should "convert to list correctly" in {
    intStream.toList should equal(List(1, 2, 3, 4, 5))
  }

  it should "take correctly" in {
    intStream.take(2).toList should equal(List(1, 2))

    Stream("one", "two", "three", "four", () => throw new Exception("shouldn't fail"))
      .take(4).toList should equal(List("one", "two", "three", "four"))
  }

  it should "drop correctly" in {
    intStream.drop(3).toList should equal(List(4, 5))

    Stream("one", "two", () => throw new Exception("shouldn't fail"), "four", "five", "six")
      .drop(3).toList should equal(List("four", "five", "six"))
  }

  it should "forAll correctly" in {
    intStream.forAll(_ < 10) should equal(true)

    Stream(1, 2, () => throw new Exception("shouldn't fail"))
      .forAll(_.toString == "1") should equal(false)
  }

  it should "takeWhile correctly" in {
    intStream.takeWhile(_ < 10).toList should equal(List(1, 2, 3, 4, 5))

    Stream(1, 2, () => throw new Exception("shouldn't fail"))
      .takeWhile(_.toString == "1").toList should equal(List(1))
  }

  it should "headOption correctly" in {
    intStream.headOption should equal(Some(1))

    Stream(1, () => throw new Exception("shouldn't fail"))
      .headOption should equal(Some(1))

    Stream.empty[Int].headOption should equal(None)
  }

  it should "map correctly" in {
    intStream.map(_ * 10).toList should equal(List(10, 20, 30, 40, 50))

    Stream(2, 4, 6, 8, 10).map(_ / 2).toList should equal(List(1, 2, 3, 4, 5))

    Stream.empty[Int].map(_.toString) should equal(Empty)
  }

  it should "filter correctly" in {
    intStream.filter(_ < 3).toList should equal(List(1, 2))

    Stream(2, 3, 4, 7, 5, 6, 8, 10).filter(_ % 2 == 0).toList should equal(List(2, 4, 6, 8, 10))

    Stream.empty[Int].filter(a => true) should equal(Empty)
  }

  it should "append correctly" in {
    intStream.append(10).toList should equal(List(1, 2, 3, 4, 5, 10))

    Stream(1, 2, 3, 4).append("five").toList should equal(List(1, 2, 3, 4, "five"))

    Stream.empty[Int].append(true).toList should equal(List(true))
  }

  it should "flatMap correctly" in {
    intStream.flatMap(a => Stream(a, a)).toList should equal(List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5))

    Stream(1, 2, 3, 4).flatMap(a => Empty) should equal(Empty)

    Stream.empty[Int].flatMap(a => Stream(1)) should equal(Empty)

    Stream(1, 2, 3, 4).flatMap(a => Stream(1)).toList should equal(List(1, 1, 1, 1))
  }

}
