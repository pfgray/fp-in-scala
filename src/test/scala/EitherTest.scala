package net.paulgray.chapter3

import java.util.logging.Logger

import org.scalatest._

/**
 * Created by pgray on 5/1/15.
 */
class EitherTest extends FlatSpec with Matchers {

  val log = Logger.getLogger(classOf[EitherTest].getName)

  def right(s: String): Either[String, String] = Right(s)
  def left(s: String): Either[String, String] = Left(s)

  "Either" should "flatMap correctly" in {
    (for {
      str <- right("lol")
    } yield str + "yes") should equal(Right("lolyes"))

    (for {
      str <- left("lol")
    } yield str + "no") should equal(Left("lol"))

    (for {
      str <- right("lol")
      str2 <- right("yes")
    } yield str + str2) should equal(Right("lolyes"))
  }

  it should "map correctly" in {
    right("1") map Integer.valueOf should equal(Right(1))

    left("1") map String.valueOf should equal(Left("1"))
  }

  it should "orElse correctly" in {
    right("lol") orElse {
      throw new Exception("should not be called")
    } should equal(Right("lol"))

    right("yes") orElse right("no") should equal(Right("yes"))
    right("yes") orElse left("no") should equal(Right("yes"))
    left("no") orElse right("yes") should equal(Right("yes"))
    left("no") orElse left("yes") should equal(Left("yes"))
  }

  it should "map2 correctly" in {
    right("lol").map2(right("yes")) { (a, b) =>
      b + a
    } should equal(Right("yeslol"))

    right("no").map2(left("yes")) { (a, b) =>
      b + a
    } should equal(Left("yes"))

    left("yes").map2(right("no")) { (a, b) =>
      b + a
    } should equal(Left("yes"))

    left("yes").map2(left("no")) { (a, b) =>
      b + a
    } should equal(Left("yes"))
  }

  it should "map2Again correctly" in {
    right("lol").map2Again(right("yes")) { (a, b) =>
      b + a
    } should equal(Right("yeslol"))

    right("no").map2Again(left("yes")) { (a, b) =>
      b + a
    } should equal(Left("yes"))

    left("yes").map2Again(right("no")) { (a, b) =>
      b + a
    } should equal(Left("yes"))

    left("yes").map2Again(left("no")) { (a, b) =>
      b + a
    } should equal(Left("yes"))
  }

}
