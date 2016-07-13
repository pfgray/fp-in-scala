package chapter3

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by nicole on 7/12/16.
 */
class TreeTest extends FlatSpec with Matchers {

  val testTree: Tree[Int] =
    Branch(
      Branch(
        Branch(
          Leaf(5),
          Leaf(6)
        ),
        Leaf(8)
      ),
      Branch(
        Leaf(9),
        Leaf(10)
      )
    )

  "Tree" should "compute the size of a tree correctly" in {
    Tree.size(testTree) should equal(9)

    Tree.size(Leaf("test")) should equal(1)
  }

  it should "find the maximum of a tree correctly" in {
    Tree.maximum(testTree) should equal(10)

    Tree.maximum(Leaf(0)) should equal(0)
  }

  it should "find the depth of a tree correctly" in {
    Tree.depth(testTree) should equal(4)

    Tree.depth(Leaf("test")) should equal(1)
  }

  it should "map a tree correctly" in {
    val smallTree = Branch(
      Leaf(5),
      Branch(
        Leaf(10),
        Leaf(4)
      )
    )

    Tree.map(smallTree)(_.toString) should equal(Branch(
      Leaf("5"),
      Branch(
        Leaf("10"),
        Leaf("4")
      )
    ))

    Tree.map(Leaf("2"))(Integer.valueOf) should equal(Leaf(2))
  }

  it should "fold a tree correctly" in {
    Tree.fold(testTree)(a => a)(_ + _) should equal(5 + 6 + 8 + 9 + 10)
  }

  it should "compute the size of a tree with fold correctly" in {
    Tree.sizeWithFold(testTree) should equal(9)

    Tree.sizeWithFold(Leaf("test")) should equal(1)
  }

  it should "find the maximum of a tree with fold correctly" in {
    Tree.maximumWithFold(testTree) should equal(10)

    Tree.maximumWithFold(Leaf(0)) should equal(0)
  }

  it should "find the depth of a tree with fold correctly" in {
    Tree.depthWithFold(testTree) should equal(4)

    Tree.depthWithFold(Leaf("test")) should equal(1)
  }

  it should "map a tree with fold correctly" in {
    val smallTree = Branch(
      Leaf(5),
      Branch(
        Leaf(10),
        Leaf(4)
      )
    )

    Tree.mapWithFold(smallTree)(_.toString) should equal(Branch(
      Leaf("5"),
      Branch(
        Leaf("10"),
        Leaf("4")
      )
    ))

    Tree.mapWithFold(Leaf("2"))(Integer.valueOf) should equal(Leaf(2))
  }

}
