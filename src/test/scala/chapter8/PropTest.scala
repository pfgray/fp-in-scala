package chapter8

import chapter6.SimpleRNG
import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by paul on 8/24/16.
  */
class PropTest extends FlatSpec with Matchers {

  val failing = Prop((tc, rng) => {
    Falsified("failed", 0)
  })
  val passing = Prop((tc, rng) => Passed)

  "Prop" should "&& correctly" in {
    (failing && passing).compute should equal(Falsified("failed", 0))

    (passing && passing).compute should equal(Passed)

    (failing && failing).compute should equal(Falsified("failed", 0))

    (passing && failing).compute should equal(Falsified("failed", 0))
  }

  "Prop" should "|| correctly" in {
    (failing || passing).run(0, SimpleRNG(1L)) should equal(Passed)

    (passing || passing).compute should equal(Passed)

    (failing || failing).compute should equal(Falsified("failed", 0))

    (passing || failing).compute should equal(Passed)
  }

  implicit class PropWithCompute(prop: Prop) {
    def compute: Result =
      prop.run(0, SimpleRNG(1L))
  }

}
