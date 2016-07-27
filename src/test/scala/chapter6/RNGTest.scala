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

    val (n2, _) = RNG.nonNegativeInt(rng2)
    n2 shouldBe positive
  }

  it should "generate non negative doubles correctly" in {
    val (n, rng2) = RNG.double(rng)
    assert(n < 1 && n > 0)

    val (n2, _) = RNG.double(rng2)
    assert(n2 < 1 && n2 > 0)
  }

  it should "generate an integer and a double correctly" in {
    val ((i, d), _) = RNG.intDouble(rng)
    assert(i > 0)
    assert(d > 0 && d < 1)
  }

  it should "generate a double and an integer correctly" in {
    val ((d, i), _) = RNG.doubleInt(rng)
    assert(i > 0)
    assert(d > 0 && d < 1)
  }

  it should "generate three doubles correctly" in {
    val ((n1, n2, n3), _) = RNG.double3(rng)
    assert(List(n1, n2, n3).forall { n =>
      n < 1 && n > 0
    })
  }

  it should "generate a list of nonNegative integers correctly" in {
    val (list1, rng2) = RNG.ints(5)(rng)
    list1.length should equal(5)

    val (list2, _) = RNG.ints(10)(rng2)
    list2.length should equal(10)
  }

  it should "generate doubles with map correctly" in {
    val (n, rng2) = RNG.double2(rng)
    assert(n < 1 && n > 0)

    val (n2, _) = RNG.double2(rng2)
    assert(n2 < 1 && n2 > 0)
  }

  it should "generate map 2 values correctly" in {
    val getTup = RNG.map2(RNG.nonNegativeInt, RNG.nonNegativeInt) { (a, b) =>
      (a, b)
    }

    val ((a, b), rng2) = getTup(rng)
  }

}
