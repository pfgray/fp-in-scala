package chapter8

import chapter6.{State, RNG}

/**
  * Created by paul on 8/22/16.
  */
case class Gen[A](sample: State[RNG, A]) {

  //8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def listOfN(n: Gen[Int]): Gen[List[A]] =
    n.flatMap(Gen.listOfN(_, this))

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
}
