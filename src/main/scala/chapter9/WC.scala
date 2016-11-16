package chapter9

/**
  * Created by paul on 10/4/16.
  */
sealed trait WC
case class Stub(s: String) extends WC
case class Part(l: String, count: Int, r: String) extends WC

trait Monoid[A] {
  def op(a: A, b: A): A
  val zero: A
}

object WC {
  val monoid = new Monoid[WC] {
    override def op(a: WC, b: WC): WC = ???

    override val zero: WC = Stub("")
  }
}
