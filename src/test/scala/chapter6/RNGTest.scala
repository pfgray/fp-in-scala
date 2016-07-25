package chapter6

import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by nicole on 7/24/16.
 */
class RNGTest extends FlatSpec with Matchers {

  val rng = SimpleRNG(1L)

  val even = new BeMatcher[Int] {
    override def apply(left: Int): MatchResult = {
      MatchResult(left % 2 == 0, s"$left is not even", s"$left is even?")
    }
  }

  val positive = new BeMatcher[Int] {
    override def apply(left: Int): MatchResult = {
      MatchResult(left > 0, s"$left is not even", s"$left is even?")
    }
  }

  "RNG" should "generate non negative integers correctly" in {
    val (n, rng2) = RNG.nonNegativeInt(rng)
    n shouldBe positive
  }

}
