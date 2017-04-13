package chapter10

import chapter7.Par
import chapter7.Par.{Par, ParExtensions}
import chapter8._

/**
 * Created by nicole on 10/4/16.
 */
trait Monoid[A] {

  def op(a: A, b: A): A
  def zero: A

}

object Monoid {

  // 10.1
  val intAddition = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a + b
    override def zero: Int = 0
  }

  // 10.2
  val intMultiplication = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a * b
    override def zero: Int = 1
  }

  // 10.3
  val booleanOr = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean) = a || b
    def zero: Boolean = false
  }

  // 10.4
  val booleanAnd = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean): Boolean = a && b
    def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a: Option[A], b: Option[A]) = a orElse b
    val zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a: A => A, b: A => A): A => A = a compose b
    val zero: A => A = a => a
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val associative: Prop = Prop {
      (max, n, rng) =>
        val test = for {
          x <- gen
          y <- gen
          z <- gen
        } yield {
          if(m.op(x, m.op(y, z)) == m.op(m.op(x, y), z)){
            Proved
          } else {
            Falsified("This monad's op is not associative", 0)
          }
        }

        val (result, _) = test.sample.run(rng)
        result
    }

    val rightId = Prop {
      (max, n, rng) =>
        gen.map(a => {
          if(m.op(a, m.zero) == a) {
            Proved
          } else {
            Falsified("This monad's identity is not right-assoviative.", 0)
          }
        }).sample.run(rng)._1
    }

    val leftId = Prop {
      (max, n, rng) =>
        gen.map(a => {
          if(m.op(m.zero, a) == a) {
            Proved
          } else {
            Falsified("This monad's identity is not left-associative.", 0)
          }
        }).sample.run(rng)._1
    }

    associative && rightId && leftId
  }

  // 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).fold(m.zero)(m.op)

  // 10.6
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, new Monoid[B => B] {
      def op(a: (B) => B, b: (B) => B): (B) => B = a compose b
      def zero: B => B = identity
    })(a => b => f(b, a))(z)

  // 10.7
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B):B =
    v match {
      case IndexedSeq() =>
        m.zero
      case IndexedSeq(h) =>
        f(h)
      case as: IndexedSeq[A] =>
        val (l, r) = as.split
        m.op(
          foldMapV(l, m)(f), foldMapV(r, m)(f)
        )
    }

  // 10.8
  def par[A](m: Monoid[A]): Monoid[Par[A]] = {
    new Monoid[Par[A]] {
      override def op(a: Par[A], b: Par[A]): Par[A] = {
        println(s"Performing op: $a, $b")
        Par.map2(a, b)(m.op)
      }

      override def zero: Par[A] = Par.unit(m.zero)
    }
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    v match {
      case IndexedSeq() =>
        println(s"returning zero...")
        par(m).zero
      case IndexedSeq(h) =>
        Par.unit(f(h))
      case as: IndexedSeq[A] =>
        println(s"parallelizing: $as")
        //now for the parallellism:
        val (l, r) = as.split
        par(m).op(
          Par.fork { parFoldMap(l, m)(f) },
          Par.fork { parFoldMap(r, m)(f) }
        )
      case _ => throw new RuntimeException(s"got: $v as unexpected value")
    }

  implicit class IndexedSeqOps[A](seq: IndexedSeq[A]) {
    def split: (IndexedSeq[A], IndexedSeq[A]) = {
      val splitted = seq.splitAt(seq.length / 2)
      println(s"splitted into: $splitted")
      splitted
    }

  }
}
