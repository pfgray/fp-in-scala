package chapter8

import chapter6.{State, SimpleRNG, RNG}
import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by paul on 8/22/16.
  */
class GenTest extends FlatSpec with Matchers {

  case class CountingGenerator(start: Int) extends RNG {
    override def nextInt: (Int, RNG) = {
      (start, CountingGenerator(start + 1))
    }
  }

  "Gen" should "choose numbers" in {
    val gen = Gen.choose(5, 13)

    val (result, _) = gen.sample.run(SimpleRNG(1L))
  }

  it should "create booleans" in {
    Gen.boolean.sample.run(CountingGenerator(0))._1 should equal(true)
    Gen.boolean.sample.run(CountingGenerator(1))._1 should equal(false)
    Gen.boolean.sample.run(CountingGenerator(2))._1 should equal(true)
  }

  it should "create a list from a generator" in {
    val gen = Gen.listOfN(10, Gen(State(_.nextInt)))

    val (result, _) = gen.sample.run(CountingGenerator(0))

    result.length should equal(10)
    result(0) should equal(0)
    result(3) should equal(3)
    result(7) should equal(7)
  }

}
