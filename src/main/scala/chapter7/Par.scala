package chapter7

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}

/**
  * Created by paul on 8/2/16.
  */
object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isCancelled: Boolean = false
    override def get(timeout: Long, unit: TimeUnit): A = get
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def isDone: Boolean = true
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        override def call(): A = a(es).get
      })

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
//      map2(pList, unit(a)) { (ls, a) =>
//        if(f(a)) a :: ls else ls
//      }
//    }
    val pars = as.map(asyncF(a => if (f(a)) List(a) else List()))
    sequence(pars) map (_.flatten)
  }

  implicit class ParExtensions[A](a: Par[A]) {
    def map[B](f: A => B): Par[B] =
      map2(a, unit())((a, _) => f(a))
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

  val maxer = (a: Int, b: Int) => {
    println(s"comparing: $a, & $b")
    a max b
  }

  implicit class ParIndexedSeqIntExtensions(as: IndexedSeq[Int]) {
    def parMax: Par[Int] =
      as.parFold(0)(maxer) // (_ max _)
  }

}
