package chapter11

import chapter7.Par
import chapter7.Par.Par

/**
  * @author pgray
  */
trait Monad[F[_]] extends Functor[F] {

  def unit[A](a: => A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))
  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma) { a =>
      map(mb) { b =>
        f(a, b)
      }
    }

}

object Monad {

  def parMonad: Monad[Par] = new Monad[Par] {
    def flatMap[A, B](fa: Par[A])(f: (A) => Par[B]) =
      fa.

    def unit[A](a: => A) = ???
  }


}
