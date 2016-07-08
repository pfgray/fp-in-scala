package chapter3

import java.util.logging.Logger

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by nicole on 7/5/16.
 */
class ListTest extends FlatSpec with Matchers {

  val log = Logger.getLogger(classOf[ListTest].getName)

  "List" should "tail correctly" in {
    List.tail(List(1, 2, 3, 4)) should equal(List(2, 3, 4))
    List.tail(List(1)) should equal(List())
    List.tail(List()) should equal(List())
  }

  it should "setHead correctly" in {
    List.setHead(List(1, 2), 1) should equal(List(1, 1, 2))
    List.setHead(List(2), 1) should equal(List(1, 2))
    List.setHead(List(), 1) should equal(List(1))
  }

  it should "drop correctly" in {
    List.drop(List(1, 2, 3, 4, 5), 2) should equal(List(3, 4, 5))
    List.drop(List(2), 1) should equal(List())
    List.drop(List(), 1) should equal(List())
    List.drop(List(1, 2), 5) should equal(List())
  }

  def lessThan(a: Int)(b: Int): Boolean = b < a
  def greaterThan(a: Int)(b: Int): Boolean = b > a

  it should "dropWhile correctly" in {
    List.dropWhile(List(1, 2, 3, 4, 5), lessThan(3)) should equal(List(3, 4, 5))
    List.dropWhile(List(3, 2, 1), greaterThan(1)) should equal(List(1))
    List.dropWhile(List(), lessThan(10)) should equal(List())
    List.dropWhile(List(1, 2), greaterThan(3)) should equal(List(1, 2))
    List.dropWhile(List(2, 1, 1, 1, 2), greaterThan(3)) should equal(List(2, 1, 1, 1, 2))
    List.dropWhile(List(4, 1, 1, 1, 2), greaterThan(3)) should equal(List(1, 1, 1, 2))
  }

  it should "init correctly" in {
    List.init(List(1, 2, 3, 4, 5)) should equal(List(1, 2, 3, 4))
    List.init(List(3, 2, 1)) should equal(List(3, 2))
    List.init(List()) should equal(List())
    List.init(List(1)) should equal(List())
    List.init(List(1, 2)) should equal(List(1))
    List.init(List(2, 1, 1, 1, 2)) should equal(List(2, 1, 1, 1))
    List.init(List(4, 1, 1, 1, 2)) should equal(List(4, 1, 1, 1))
  }

  val divide: (Float, Float) => Float = _ / _

  it should "foldRight correctly" in {
    List.foldRight(List(1f, 2f, 4f), 1f)(divide) should equal(1f / (2f / (4f / 1f)))

    List.foldRight(List(4f, 2f, 1f), 1f)(divide) should equal(4f / (2f / (1f / 1f)))
  }

  it should "compute length using foldRight correctly" in {
    List.length(List(1, 2, 3, 4, 5)) should equal(5)
    List.length(List(3, 2, 1)) should equal(3)
    List.length(List()) should equal(0)
    List.length(List(1)) should equal(1)
  }

  it should "foldLeft correctly" in {
    List.foldLeft(List(1f, 2f, 4f), 1f)(divide) should equal(((1f / 1f) / 2f) / 4f)

    List.foldLeft(List(4f, 2f, 1f), 1f)(divide) should equal(((1f / 4f) / 2f) / 1f)
  }

  it should "sum with foldLeft correctly" in {
    List.sum(List(1, 2, 4)) should equal(7)
    List.sum(List()) should equal(0)
    List.sum(List(14)) should equal(14)
  }

  it should "product with foldLeft correctly" in {
    List.product(List(1, 2, 4)) should equal(8)
    List.product(List()) should equal(1)
    List.product(List(14)) should equal(14)
    List.product(List(5, 6, 7)) should equal(5 * 6 * 7)
  }

  it should "find the length of a list with foldLeft correctly" in {
    List.len(List(1, 2, 4)) should equal(3)
    List.len(List()) should equal(0)
    List.len(List(14)) should equal(1)
    List.len(List(5, 6, 7, 5, 4, 2)) should equal(6)
  }

  it should "reverse a list with foldRight correctly" in {
    List.reverse(List(1, 2, 4)) should equal(List(4, 2, 1))
    List.reverse(Nil) should equal(Nil)
    List.reverse(List(14)) should equal(List(14))
    List.reverse(List(5, 6, 7, 5, 4, 2)) should equal(List(2, 4, 5, 7, 6, 5))
  }

  it should "foldLeft in terms of foldRight correctly" in {
    List.foldLeft2(List(1f, 2f, 4f), 1f)((a, b) => b / a) should equal(((1f / 1f) / 2f) / 4f)

    List.foldLeft2(List(4f, 2f, 1f), 1f)((a, b) => b / a) should equal(((1f / 4f) / 2f) / 1f)
  }

  it should "foldRight in terms of foldLeft correctly" in {
    List.foldRight2(List(1f, 2f, 4f), 1f)((a, b) => b / a) should equal(1f / (2f / (4f / 1f)))

    List.foldRight2(List(4f, 2f, 1f), 1f)((a, b) => b / a) should equal(4f / (2f / (1f / 1f)))
  }

}
