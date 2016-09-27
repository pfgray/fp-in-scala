package chapter8

import chapter6.{State, RNG}
import chapter5.Stream
import chapter8.Prop.{MaxSize, SuccessCount, FailedCase, TestCases}

/**
  * Created by paul on 8/22/16.
  */
case class Gen[+A](sample: State[RNG, A]) {

  def apply(rng: RNG) =
    sample.run(rng)

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

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

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  // 8.9
  def &&(p: Prop): Prop = Prop((ms, tc, rng) => {
    this.run(ms, tc, rng) match {
      case Passed => p.run(ms, tc, rng)
      case Falsified(f, s) => Falsified(f, s)
    }
  })

  def ||(p: Prop): Prop = Prop((ms, tc, rng) => {
   this.run(ms, tc, rng) match {
      case Passed => Passed
      case Falsified(f, s) => p.run(ms, tc, rng)
    }
  })

}

object Prop {
  type FailedCase = String
  type TestCases = Int
  type MaxSize = Int
  type SuccessCount = Int
  // type Result = Option[(FailedCase, SuccessCount)]

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) =>
      (randomStream(as)(rng).zip(Stream.from(0)).take(n) map {
        case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch {
            case e: Exception => Falsified(buildMsg(a, e), i)
          }
      }).find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (_,n,rng) => f(n,rng) }

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
