package chapter6

import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by paul on 7/28/16.
  */
class StateTest extends FlatSpec with Matchers {

  "State" should "do stuff" in {
    val stringGenerator: State[Int, String] = State(i => (i.toString, i + 1))

    val tupGenerator = for {
      one <- stringGenerator
      two <- stringGenerator
      three <- stringGenerator
    } yield (one, two, three)

    tupGenerator.run(1)._1 should equal(("1", "2", "3"))

    tupGenerator.run(10)._1 should equal(("10", "11", "12"))
  }

}
