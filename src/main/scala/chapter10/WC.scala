package chapter10

/**
  * Created by paul on 10/4/16.
  */
sealed trait WC
case class Stub(s: String) extends WC
case class Part(l: String, count: Int, r: String) extends WC

object WC {

  // 10.10
  val wcMonoid = new Monoid[WC] {
    def op(a: WC, b: WC): WC =
      (a, b) match {
        case (Stub(s1), Stub(s2)) =>
          Stub(s1 + s2)
        case (Stub(s), Part(l, c, r)) =>
          Part(s+l, c, r)
        case (Part(l, c, r), Stub(s)) =>
          Part(l, c, r+s)
        case (Part(ll, lc, lr), Part(rl, rc, rr)) =>
          if(lr.length > 0 || rl.length > 0) {
            Part(ll, lc + rc + 1, rr)
          } else {
            Part(ll, lc + rc, rr)
          }
      }

    val zero: WC = Stub("")
  }

  // 10.11
  def countWords(s: String): WC =
    Monoid.foldMapV(s, wcMonoid) {
      case ' ' | '\n' => Part("", 0, "")
      case c => Stub(c.toString)
    }

}
