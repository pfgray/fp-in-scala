package chapter6

/**
 * Created by nicole on 7/24/16.
 */
trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    rng.nextInt match {
      case (n, rng2) if n == Int.MinValue => nonNegativeInt(rng)
      case (n, rng2) if n < 0 => (Math.abs(n), rng2)
      case t => t
    }

   // 6.2
  def double(rng: RNG): (Double, RNG) =
    nonNegativeInt(rng) match {
      case (i, rng) if (i == Int.MaxValue) =>
        double(rng)
      case (i, rng) =>
        (i.toDouble /
          Int.MaxValue.toDouble, rng)
    }

  // 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (int, rng2) = nonNegativeInt(rng)
    val (dub, rng3) = double(rng)
    ((int, dub), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (tup, rng2) = intDouble(rng)
    (tup.swap, rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (dub1, rng1) = double(rng)
    val (dub2, rng2) = double(rng1)
    val (dub3, rng3) = double(rng2)
    ((dub1, dub2, dub3), rng3)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    count match {
      case i if i <= 0 => (Nil, rng)
      case i => {
        val (a, rng2) = nonNegativeInt(rng)
        val (list, rng3) = ints(i - 1)(rng2)
        (a :: list, rng3)
      }
    }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  // 6.5
  def double2: Rand[Double] =
    map(nonNegativeInt _)(_ / (Int.MaxValue.toDouble + 1))

  // 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](list: List[Rand[A]]): Rand[List[A]] =
    list match {
      case Nil => unit(Nil)
      case h :: t =>
        map2(h, sequence(t)) { (a, la) =>
          a :: la
        }
    }
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >> 16).toInt
    (n, nextRNG)
  }
}
