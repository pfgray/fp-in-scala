package chapter8

import chapter6.{SimpleRNG, RNG}
import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by paul on 8/29/16
  */
class SGenTest extends FlatSpec with Matchers {


  case class CountingGenerator(start: Int) extends RNG {
    override def nextInt: (Int, RNG) = {
      (start, CountingGenerator(start + 1))
    }
  }

  "SGen" should "map correctly" in {
    val sgen = Gen.choose(0, 5).unsized
    sgen.forSize(100).sample.run(CountingGenerator(0))
  }

  "SGen" should "create numerated lists" in {
    val sgen = Gen.choose(0, 5)
    val listSgen = SGen.listOf(sgen)

    val res = listSgen.forSize(8).sample.run(CountingGenerator(3))
    println(res._1)
  }

  "SGen" should "create nonempty lists" in {
    val sgen = Gen.choose(0, 5)
    val listSgen = SGen.listOf1(sgen)

    val res = listSgen.forSize(0).sample.run(CountingGenerator(3))
    res._1.length should equal(1)
  }

  "List" should "sort correctly" in {
    val listGenerator = SGen.listOf1(Gen.choose(1, 100))
    val sortedProp = Prop.forAll(listGenerator) { ns =>
      val sorted = ns.sorted
      val zipped = sorted.zip(sorted.tail)
      zipped forall { case (a, b) => a <= b }
      //true
    }

    val result = sortedProp.run(27, 5, SimpleRNG(0L))

    result should equal(Passed)

  }

}
