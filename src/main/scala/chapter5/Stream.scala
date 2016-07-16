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

}
