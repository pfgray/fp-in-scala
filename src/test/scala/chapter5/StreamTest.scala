package chapter5

import org.scalatest.{Matchers, FlatSpec}

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

}
