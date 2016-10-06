package chapter9

import chapter6.RNG
import chapter8.Prop.TestCases
import chapter8._
import org.scalatest.prop.Configuration.MaxSize

/**
 * Created by nicole on 10/4/16.
 */
trait Monoid[A] {

  def op(a: A, b: A): A
  def zero: A

}

object Monoid {

  val intAddition = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a + b
    override def zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a * b
    override def zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean) = a || b
    def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean): Boolean = a && b
    def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a: Option[A], b: Option[A]) = a orElse b
    val zero: Option = None
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
            Falsified("This monad's identity is not left-assoviative.", 0)
          }
        }).sample.run(rng)._1
    }

    associative && rightId && leftId
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A=>B): B =
    as.map(f).fold(m.zero)(m.op)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A=>B):B =
    v.split match {
      case (Nil, Nil) =>
        m.zero
      case (h+:t, Nil) =>
        m.op(f(h), foldMapV(t, m)(f))
      case (Nil, h+:t) =>
        m.op(f(h), foldMapV(t, m)(f))
      case (lh+:lt, rh+:rt) =>
        m.op(
          m.op(f(lh), foldMapV(lt, m)(f)),
          m.op(f(rh), foldMapV(rt, m)(f))
        )
    }

  implicit class IndexedSeqOps[A](seq: IndexedSeq[A]) {
    def split: (IndexedSeq[A], IndexedSeq[A]) =
      seq.splitAt(seq.length / 2)
  }
}
