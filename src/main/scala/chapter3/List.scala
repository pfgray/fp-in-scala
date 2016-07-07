package chapter3

/**
 * Created by nicole on 7/5/16.
 */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // 3.2
  def tail[A](l: List[A]) = l match {
    case Cons(h, t) => t
    case Nil => Nil
  }

  // 3.3
  def setHead[A](l: List[A], a: A) = {
    Cons(a, l)
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (_, 0) => l
    case (Nil, _) => Nil
    case (Cons(a, t), _) => drop(t, n - 1)
  }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => if(f(h)){
        dropWhile(t, f)
      } else {
        l
      }
    }
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  // 3.9
  def length[A](a: List[A]) =
    foldRight(a, 0)((a,b) => b + 1)

  // 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

}
