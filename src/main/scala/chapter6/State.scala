package chapter6

import scala.collection.concurrent.RDCSS_Descriptor

/**
  * Created by paul on 7/27/16.
  */
case class State[S, +A](run: S => (A, S)) {

  // 6.10
  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, s2) = run(s)
    (f(a), s2)
  })

  def map2[B, C](bS: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {
    val (a, s2) = run(s)
    val (b, s3) = bS.run(s2)
    (f(a, b), s3)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val(a, s2) = run(s)
    f(a).run(s2)
  })

}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](list: List[State[S, A]]): State[S, List[A]] =
    list match {
      case Nil => unit(Nil)
      case h :: t => h.map2(sequence(t))(_ :: _)
    }

}
