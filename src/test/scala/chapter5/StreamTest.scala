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

  "Stream" should "take correctly" in {
    intStream.take(2) should equal(Stream(1, 2))

    Stream("one", "two", "three", "four", throw new Exception("shouldn't fail"))
      .take(4) should equal(Stream("one", "two", "three", "four"))
  }


}
