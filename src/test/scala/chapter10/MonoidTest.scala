package chapter10

import java.util.concurrent.{Executors, ThreadLocalRandom}

import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by paul on 11/13/16.
  */
class MonoidTest extends FlatSpec with Matchers {

  val consoleIntAddition: Monoid[Int] = new Monoid[Int] {

    override def op(a: Int, b: Int): Int = {
      //val random = ThreadLocalRandom.current().nextInt(1, 5 + 1) * 100
      println(s"calcuating: ($a+$b), but waiting milliseconds")
      //Thread.sleep(random)
      a+b
    }

    override def zero: Int = 0
  }

  "Monoid" should "parallellize correctly" in {
    val seq = IndexedSeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
    val toExec = Monoid.parFoldMap(seq, consoleIntAddition)(identity)

    val sum = toExec.apply(Executors.newFixedThreadPool(1))

    println(s"got: $sum")

    println(s"got: ${Monoid.foldMapV(seq, consoleIntAddition)(identity)}")
  }



}
