package chapter8

/**
  * Created by paul on 8/29/16.
  */
case class SGen[+A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = forSize(n)

  //8.11
  def map[B](f: A => B): SGen[B] =
    SGen(forSize(_).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen { i =>
      forSize(i) flatMap { a =>
        f(a).forSize(i)
      }
    }
}

object SGen {

  //8.12
  def listOf[N](g: Gen[N]): SGen[List[N]] =
    SGen(Gen.listOfN(_, g))

  //8.13
  def listOf1[N](g: Gen[N]): SGen[List[N]] =
    SGen(i => Gen.listOfN(i max 1, g))

}
