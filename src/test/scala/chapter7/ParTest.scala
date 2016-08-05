package chapter7

import java.util.concurrent.{ForkJoinPool, ExecutorService}

import org.scalatest.{Matchers, FlatSpec}
import chapter7.Par.ParExtensions

/**
 * Created by nicole on 8/4/16.
 */
class ParTest  extends FlatSpec with Matchers {

  "Par" should "sequence correctly" in {
    val pars = Par.sequence(List(
      Par.unit(30),
      Par.unit(5),
      Par.unit(6)
    ))

    val sum = pars.map(_.reduce(_ / _))

    sum(new ForkJoinPool(1)).get() should equal(1)

  }

}
