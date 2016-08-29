package chapter8

import chapter6.{State, RNG}
import chapter8.Prop.{SuccessCount, FailedCase, TestCases}

/**
  * Created by paul on 8/22/16.
  */
case class Gen[+A](sample: State[RNG, A]) {

  // 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def listOfN(n: Gen[Int]): Gen[List[A]] =
    n.flatMap(Gen.listOfN(_, this))

  // 8.10
  def unsized: SGen[A] =
    SGen(i => this)

}

object Gen {

  // 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  // 8.5
  def unit[A](a: A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.nonNegativeInt).map(_ % 2).map(n => if(n == 1) false else true))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    if(n <= 0) {
      Gen.unit(Nil)
    } else {
      Gen(g.sample.map2(listOfN(n - 1, g).sample)((a, lis) => a :: lis))
    }

  // 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean flatMap {
      case true => g1
      case false => g2
    }

  // 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    Gen(State(RNG.double)) flatMap { dub =>
      dub * (g1._2 + g2._2) match {
        case a if a <= g1._2 => g1._1
        case _ => g2._1
      }
    }
}

case class Prop(run: (TestCases, RNG) => Result) {
  // 8.9
  def &&(p: Prop): Prop = Prop((tc, rng) => {
    this.run(tc, rng) match {
      case Passed => p.run(tc, rng)
      case Falsified(f, s) => Falsified(f, s)
    }
  })

  def ||(p: Prop): Prop = Prop((tc, rng) => {
   this.run(tc, rng) match {
      case Passed => Passed
      case Falsified(f, s) => p.run(tc, rng)
    }
  })

}

object Prop {
  type FailedCase = String
  type TestCases = Int
  type SuccessCount = Int
  // type Result = Option[(FailedCase, SuccessCount)]
}

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  override def isFalsified: Boolean = false
}
case class Falsified(failure: FailedCase, successCount: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}
