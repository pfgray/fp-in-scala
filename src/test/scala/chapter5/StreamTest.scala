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

  it should "constant correctly" in {
    Stream.constant("thing").take(5).toList should equal(List("thing", "thing", "thing", "thing", "thing"))

    Stream.constant(1).take(0).toList should equal(Nil)

    Stream.constant(5).take(3).toList should equal(List(5, 5, 5))
  }

  it should "from correctly" in {
    Stream.from(10).take(5).toList should equal(List(10, 11, 12, 13, 14))

    Stream.from(100).take(0) should equal(Empty)

    Stream.from(1000).take(3).toList should equal(List(1000, 1001, 1002))
  }

  it should "fibs correctly" in {
    Stream.fibs.take(8).toList should equal(List(0, 1, 1, 2, 3, 5, 8, 13))

    Stream.fibs.take(0) should equal(Empty)
  }

  it should "unfold correctly" in {
    Stream.unfold(10)(a => None).take(8) should equal(Empty)

    val umm = Stream.unfold(0)(num => {
      if(num == 0) { Some(("yes", 1)) } else { Some(("no", 0)) }
    }).take(5).toList should equal(List("yes", "no", "yes", "no", "yes"))
  }

  it should "fibs with unfold correctly" in {
    Stream.fibs2.take(8).toList should equal(List(0, 1, 1, 2, 3, 5, 8, 13))

    Stream.fibs2.take(0) should equal(Empty)
  }

  it should "from with unfold correctly" in {
    Stream.from2(0).take(8).toList should equal(List(0, 1, 2, 3, 4, 5, 6, 7))

    Stream.from2(5).take(5).toList should equal(List(5, 6, 7, 8, 9))

    Stream.from2(100).take(0) should equal(Empty)
  }

  it should "constant with unfold correctly" in {
    Stream.constant2("thing").take(5).toList should equal(List("thing", "thing", "thing", "thing", "thing"))

    Stream.constant2(1).take(0).toList should equal(Nil)

    Stream.constant2(5).take(3).toList should equal(List(5, 5, 5))
  }

  it should "ones with unfold correctly" in {
    Stream.ones2.take(8).toList should equal(List(1, 1, 1, 1, 1, 1, 1, 1))

    Stream.ones2.take(0) should equal(Empty)
  }

  it should "map with unfold correctly" in {
    intStream.map2(_ * 10).toList should equal(List(10, 20, 30, 40, 50))

    Stream(2, 4, 6, 8, 10).map2(_ / 2).toList should equal(List(1, 2, 3, 4, 5))

    Stream.empty[Int].map2(_.toString) should equal(Empty)
  }

  it should "take with unfold correctly" in {
    intStream.take2(2).toList should equal(List(1, 2))

    Stream("one", "two", "three", "four", () => throw new Exception("shouldn't fail"))
      .take2(4).toList should equal(List("one", "two", "three", "four"))
  }

  it should "takeWhile with unfold correctly" in {
    intStream.takeWhile2(_ < 10).toList should equal(List(1, 2, 3, 4, 5))

    Stream(1, 2, () => throw new Exception("shouldn't fail"))
      .takeWhile2(_.toString == "1").toList should equal(List(1))
  }

  it should "zipWith with unfold correctly" in {
    intStream.zipWith(intStream)(_ * _).toList should equal(List(1, 4, 9, 16, 25))

    val strings = Stream("one", "two", "three", "four")
    strings.zipWith(intStream)(_ + String.valueOf(_)).toList should equal(List("one1", "two2", "three3", "four4"))
  }

  it should "zipAll with unfold correctly" in {
    intStream.zipAll(intStream).toList should equal(List(
      (Some(1), Some(1)),
      (Some(2), Some(2)),
      (Some(3), Some(3)),
      (Some(4), Some(4)),
      (Some(5), Some(5))
    ))

    val strings = Stream("one", "two", "three", "four")
    strings.zipAll(intStream).toList should equal(List(
      (Some("one"), Some(1)),
      (Some("two"), Some(2)),
      (Some("three"), Some(3)),
      (Some("four"), Some(4)),
      (None, Some(5))
    ))

    intStream.zipAll(strings).toList should equal(List(
      (Some(1), Some("one")),
      (Some(2), Some("two")),
      (Some(3), Some("three")),
      (Some(4), Some("four")),
      (Some(5), None)
    ))
  }

}
