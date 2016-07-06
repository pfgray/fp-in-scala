package chapter4

sealed trait Either[+E, +A] {

  val self = this

  def map[B](f: A => B): Either[E, B] =
    self match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

  def flatMap[EE >: E, B](f:A => Either[EE, B]): Either[EE, B] =
    self match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    self match {
      case Left(e) => b
      case Right(a) => Right(a)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    (self, b) match {
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Left(e), _) => Left(e)
      case (_, Left(ee)) => Left(ee)
    }
  }

  def map2Again[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- self
      bRight <- b
    } yield f(a, bRight)

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]