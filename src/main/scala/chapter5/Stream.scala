package chapter5

import Stream._

/**
 * Created by nicole on 7/13/16.
 */
sealed trait Stream[+A] {

  // 5.1
  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => List(h()) ::: t().toList
    }

  // 5.2
  def take(n : Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, t) if n == 1 => cons(h(), empty)
      case _ => Empty
    }

  // 5.3
  def drop(n: Int): Stream[A] =
    (n, this) match {
      case (0, s) => s
      case (n, Cons(h, t)) => t().drop(n - 1)
      case _ => Empty
    }

  def exists(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // 5.5
  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (a, b) =>
      if (p(a)) cons(a, b) else Empty
    }

  // 5.6
  def headOption: Option[A] =
    foldRight(None: Option[A]) { (a, b) =>
      Some(a)
    }

  // 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B]) { (a, b) =>
      cons(f(a), b)
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (a, b) =>
      if (p(a)) cons(a, b) else b
    }

  def append[B >: A](bb: => B): Stream[B] =
    foldRight(cons[B](bb, empty)) { (a, b) =>
      cons(a, b)
    }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]) { (a, b) =>
      f(a).foldRight(b) { (a2, b2) =>
        cons(a2, b2)
      }
    }

  // 5.13
  def map2[B](f: A => B): Stream[B] =
    unfold(0) { n =>
      drop(n).headOption.map(a => (f(a), n + 1))
    }

  def take2(n: Int): Stream[A] =
    unfold(0) { s =>
      if(s < n) {
        drop(s).headOption.map((_, s + 1))
      } else {
        None
      }
    }

  def takeWhile2(f: A => Boolean): Stream[A] =
    unfold(0) { n =>
      drop(n).headOption.filter(f).map((_, n + 1))
    }

  def zipWith[B, C](r: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold(0) { n =>
      for {
        a <- drop(n).headOption
        b <- r.drop(n).headOption
      } yield (f(a, b), n + 1)
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(0) { n =>
      (drop(n).headOption, s2.drop(n).headOption) match {
        case (None, None) => None
        case tup => Some((tup, n + 1))
      }
    }

//  def hasSubsequence(sub: Stream[A]): Boolean =
//    unfold(sub) { sub =>
//
//    }

  // 5.14
  def startsWith[B >: A](sub: Stream[B]): Boolean =
    zipWith(sub)(_ == _).forAll(_ == true)

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  // 5.8
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  // 5.9
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  // 5.10
  def fibs = fibsFrom(0, 1): Stream[Int]

  def fibsFrom(n1: Int, n2: Int): Stream[Int] =
    cons(n1, fibsFrom(n2, n1 + n2))

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).fold(empty[A]) { case (a, s) =>
      cons(a, unfold(s)(f))
    }

  // 5.12
  def fibs2 =
    unfold((0, 1)) { case(n1, n2) =>
      Some((n1, (n2, n2 + n1)))
    }

  def from2(n: Int): Stream[Int] =
    unfold(n) { n =>
      Some((n, n + 1))
    }

  def constant2[A](a: A): Stream[A] =
    unfold(a)(a => Some((a, a)))

  def ones2: Stream[Int] =
    unfold(1)(a => Some((1, 1)))

}
