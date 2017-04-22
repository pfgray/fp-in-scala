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

  // 11.3
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma match {
      case Nil => unit(Nil)
      case h :: t =>
        map2(h, sequence(t)) { (a, la) =>
          a :: la
        }
    }

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    sequence(la.map(f))


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

  import chapter6.State

  // 11.2
  class StateMonad[A] extends Monad[({type λ[α] = State[A, α]})#λ] {
    def unit[B](b: => B): State[A, B] = State.unit(b)

    def flatMap[B, C](fa: State[A, B])(f: B => State[A, C]): State[A, C] =
      fa flatMap f
  }

}
