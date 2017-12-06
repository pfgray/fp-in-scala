package chapter2

import java.util.logging.Logger

class MyModule
object MyModule {
  val log = Logger.getLogger(classOf[MyModule].getName)

  // 2.1
  def fib(n: Int): Int = {
    log.info(s"Calling fib with $n")
    @annotation.tailrec
    def go(current: Int, acc: Int, i: Int): Int = {
      if (i == 1)
        current
      else
        go(acc, acc + current, i - 1)
    }

    go(0, 1, n)
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(prev: A, rest: Array[A]): Boolean =
      rest.headOption match {
        case None => true
        case Some(next) =>
          if(ordered(prev, next)){
            go(next, rest.tail)
          } else {
            false
          }
      }
    go(as.head, as.tail)
  }

  // 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

}
