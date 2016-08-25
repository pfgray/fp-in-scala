package chapter7

import java.util.concurrent.{Executors, ForkJoinPool, ExecutorService}

import org.scalatest.{Matchers, FlatSpec}
import chapter7.Par.{ParExtensions, ParIndexedSeqIntExtensions}

/**
 * Created by nicole on 8/4/16.
 */
class ParTest extends FlatSpec with Matchers {

  "Par" should "sequence correctly" in {
    val pars = Par.sequence(List(
      Par.unit(30),
      Par.unit(5),
      Par.unit(6)
    ))

    val sum = pars.map(_.reduce(_ / _))

    sum(new ForkJoinPool(1)).get() should equal(1)
  }

  it should "filter lists correctly" in {
    val things = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    val filterPar = Par.parFilter(things)(_ > 7)

    val res = filterPar(Executors.newFixedThreadPool(4))
    val filtered = res.get()
    filtered should equal(List(8, 9, 10))
  }

  it should "dead-lock with certain fixed sized threadpools" in {
    val things = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    val filterPar = Par.parFilter(things)(_ > 7)

    val res = filterPar(Executors.newFixedThreadPool(4))
    val filtered = res.get()
    filtered should equal(List(8, 9, 10))
  }

  it should "do other stuff" in {
    val thing = IndexedSeq(4, 5, 3, 6, 8, 9, 12, 4, 5, 2, 1)

    thing.parMax(new ForkJoinPool(10))
  }

}
