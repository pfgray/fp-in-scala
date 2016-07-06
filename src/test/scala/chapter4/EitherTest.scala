package chapter4

import java.util.logging.Logger

import org.scalatest._

/**
 * Created by pgray on 5/1/15.
 */
class EitherTest extends FlatSpec with Matchers {

  val log = Logger.getLogger(classOf[EitherTest].getName)

  def right(s: String): chapter4.Either[String, String] = chapter4.Right(s)
  def left(s: String): chapter4.Either[String, String] = chapter4.Left(s)

  "Either" should "flatMap correctly" in {
    (for {
      str <- right("lol")
    } yield str + "yes") should equal(chapter4.Right("lolyes"))

    (for {
      str <- left("lol")
    } yield str + "no") should equal(chapter4.Left("lol"))

    (for {
      str <- right("lol")
      str2 <- right("yes")
    } yield str + str2) should equal(chapter4.Right("lolyes"))
  }

  it should "map correctly" in {
    right("1") map Integer.valueOf should equal(chapter4.Right(1))

    left("1") map String.valueOf should equal(chapter4.Left("1"))
  }

  it should "orElse correctly" in {
    right("lol") orElse {
      throw new Exception("should not be called")
    } should equal(chapter4.Right("lol"))

    right("yes") orElse right("no") should equal(chapter4.Right("yes"))
    right("yes") orElse left("no") should equal(chapter4.Right("yes"))
    left("no") orElse right("yes") should equal(chapter4.Right("yes"))
    left("no") orElse left("yes") should equal(chapter4.Left("yes"))
  }

  it should "map2 correctly" in {
    right("lol").map2(right("yes")) { (a, b) =>
      b + a
    } should equal(chapter4.Right("yeslol"))

    right("no").map2(left("yes")) { (a, b) =>
      b + a
    } should equal(chapter4.Left("yes"))

    left("yes").map2(right("no")) { (a, b) =>
      b + a
    } should equal(chapter4.Left("yes"))

    left("yes").map2(left("no")) { (a, b) =>
      b + a
    } should equal(chapter4.Left("yes"))
  }

  it should "map2Again correctly" in {
    right("lol").map2Again(right("yes")) { (a, b) =>
      b + a
    } should equal(chapter4.Right("yeslol"))

    right("no").map2Again(left("yes")) { (a, b) =>
      b + a
    } should equal(chapter4.Left("yes"))

    left("yes").map2Again(right("no")) { (a, b) =>
      b + a
    } should equal(chapter4.Left("yes"))

    left("yes").map2Again(left("no")) { (a, b) =>
      b + a
    } should equal(chapter4.Left("yes"))
  }

}
