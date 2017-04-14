package chapter10

import chapter3.{Branch, Leaf, Tree}
import chapter5.{Cons, Empty, Stream}


/**
  * @author pgray
  */
trait Foldable[F[_]] {

  def foldRight[A, B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B,A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  //10.15
  def toList[A](fs: F[A]): List[A] =
    foldLeft[A, List[A]](fs)(Nil) {
      (b, a) => a :: b
    }

}

object Foldable {

  // 10.12
  val foldableList: Foldable[List] = new Foldable[List] {

    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
      as match {
        case Nil => z
        case head :: tail => f(foldLeft(tail)(z)(f), head)
      }

    def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero) {
        (b, a) => mb.op(b, f(a))
      }

    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
      as match {
        case Nil => z
        case head :: tail => f(head, foldRight(tail)(z)(f))
      }
  }

  val foldableIndexedSeq: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
    def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
      as match {
        case IndexedSeq() => z
        case iSeq: IndexedSeq[A] =>
          f(foldLeft(iSeq.tail)(z)(f), iSeq.head)
      }

    def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(mb: Monoid[B]) =
      foldLeft(as)(mb.zero) {
        (b, a) => mb.op(b, f(a))
      }

    def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
      as match {
        case IndexedSeq() => z
        case iSeq: IndexedSeq[A] => f(iSeq.head, foldRight(iSeq.tail)(z)(f))
      }
  }

  val foldableStream: Foldable[Stream] = new Foldable[Stream] {
    def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
      as match {
        case Empty => z
        case Cons(head, tail) => f(foldLeft(tail())(z)(f), head())
      }

    def foldMap[A, B](as: Stream[A])(f: (A) => B)(mb: Monoid[B]) =
      foldLeft(as)(mb.zero) {
        (b, a) => mb.op(b, f(a))
      }

    def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
      as match {
        case Empty => z
        case Cons(head, tail) => f(head(), foldRight(tail())(z)(f))
      }
  }

  // 10.13
  val foldableTree: Foldable[Tree] = new Foldable[Tree] {
    def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Leaf(a) => f(z, a)
        case Branch(l, r) =>
          foldLeft(l)(foldLeft(r)(z)(f))(f)
      }

    def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]) =
      foldLeft(as)(mb.zero) {
        (b, a) => mb.op(b, f(a))
      }

    def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Leaf(a) => f(a, z)
        case Branch(l, r) =>
          foldRight(r)(foldRight(l)(z)(f))(f)
      }
  }

  // 10.14
  val foldOption: Foldable[Option] = new Foldable[Option] {
    def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      as match {
        case None => z
        case Some(a) => f(z, a)
      }

    def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero) {
        (b, a) => mb.op(b, f(a))
      }

    def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
      foldLeft(as)(z) { (b, a) => f(a, b) }
  }

}