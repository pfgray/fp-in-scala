package chapter10

import java.util.concurrent.{Executors, ThreadLocalRandom}

import chapter7.Par
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by paul on 11/13/16.
  */
class MonoidTest extends FlatSpec with Matchers {

  val consoleIntAddition: Monoid[Int] = new Monoid[Int] {

    override def op(a: Int, b: Int): Int = {
      val random = ThreadLocalRandom.current().nextInt(1, 5 + 1) * 100
      Thread.sleep(random)
      a+b
    }

    override def zero: Int = 0
  }

  "Monoid" should "parallellize correctly" in {
    val es = Executors.newFixedThreadPool(2)
    val seq = IndexedSeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
    val toExec = Monoid.parFoldMap(seq, consoleIntAddition)(identity)

    Par.run(es)(toExec) should be(120)

  }



}
