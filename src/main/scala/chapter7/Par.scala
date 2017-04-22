package chapter7

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent._

/**
  * Created by paul on 8/2/16.
  */
object Par {

  sealed trait Future[+A] {
    private[chapter7] def apply(cb: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit) = cb(a)
    }

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown() }
    latch.await
    ref.get
  }

  def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      def apply(cb: (C) => Unit) = {
        var ar: Option[A] = None
        var br: Option[B] = None

        val combiner = Actor[Either[A, B]](es) {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(cb(f(a, b)))
          }
          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(cb(f(a, b)))
          }
        }

        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit) =
        eval(es)(a(es)(cb))
    }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

  // 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = a => fork(unit(f(a)))

  // 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(Nil: List[A])) { (par, l) =>
      map2(par, l)(_ :: _)
    }

  // 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
//    as.foldRight(unit(Nil: List[A])) { (a, pList) =>
//      map2(pList, unit(a)) { (ls, a) =

//        if(f(a)) a :: ls else ls
//      }
//    }
    val pars = as.map(asyncF(a => if (f(a)) List(a) else List()))
    sequence(pars) map (_.flatten)
  }

  implicit class ParExtensions[A](a: Par[A]) {
    def map[B](f: A => B): Par[B] =
      map2(a, unit())((a, _) => f(a))

    def flatMap[B](f: A => Par[B]): Par[B] =
      es => f(a(es))(es)

  }

  implicit class ParIndexedSeqExtensions[A](as: IndexedSeq[A]) {
    // 7.8
    def parFold(z: A)(f: (A, A) => A): Par[A] =
      if (as.length <= 1) {
        Par.unit[A](as.headOption.getOrElse(z))
      } else {
        val (l, r) = as.splitAt(as.length / 2)
        Par.map2(Par.fork(l.parFold(z)(f)), Par.fork(r.parFold(z)(f)))(f)
      }
  }

  implicit class ParIndexedSeqIntExtensions(as: IndexedSeq[Int]) {
    def parMax: Par[Int] =
      as.parFold(0)(_ max _)
  }

}
