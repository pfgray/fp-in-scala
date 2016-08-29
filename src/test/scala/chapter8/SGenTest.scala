package chapter8

import chapter6.RNG
import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by paul on 8/29/16.
  */
class SGenTest extends FlatSpec with Matchers {


  case class CountingGenerator(start: Int) extends RNG {
    override def nextInt: (Int, RNG) = {
      println(s"got request for: ${start}")
      (start, CountingGenerator(start + 1))
    }
  }

  "SGen" should "map correctly" in {
    val sgen = Gen.choose(0, 5).unsized
    sgen.forSize(100).sample.run(CountingGenerator(0))
  }

}
