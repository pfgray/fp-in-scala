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

  // 11.1
  def parMonad: Monad[Par] = new Monad[Par] {
    import chapter7.Par.ParExtensions
    def flatMap[A, B](fa: Par[A])(f: (A) => Par[B]) =
      fa flatMap f

    def unit[A](a: => A) = Par.unit(a)
  }

  def optionMonad: Monad[Option] = new Monad[Option] {
    def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]) =
      fa flatMap f

    def unit[A](a: => A) = Some(a)
  }

  import chapter5.Stream

  def streamMonad: Monad[Stream] = new Monad[Stream] {
    def flatMap[A, B](fa: Stream[A])(f: (A) => Stream[B]) =
      fa flatMap f

    override def unit[A](a: => A) = Stream(a)
  }

  import chapter3.List

  def listMonad: Monad[List] = new Monad[List] {
    def flatMap[A, B](fa: List[A])(f: (A) => List[B]) =
      List.flatMap(fa)(f)

    def unit[A](a: => A) = List(a)
  }

}
