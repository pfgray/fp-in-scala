package chapter5

/**
 * Created by nicole on 7/13/16.
 */
sealed trait Stream[+A] {

  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => List(h()) ::: t().toList
    }

  def take(n : Int): Stream[A] =
    (n, this) match {
      case (n, Cons(h, t)) if n > 0 =>
        Cons(h, () => t().take(n - 1))
      case _ => Empty
    }

  def drop(n: Int): Stream[A] =
    (n, this) match {
      case (0, Cons(h, t)) => t()
      case (n, Cons(h, t)) => t().drop(n-1)
      case _ => Empty
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


