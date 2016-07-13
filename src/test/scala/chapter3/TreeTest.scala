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

  "Tree" should "find the maximum of a tree correctly" in {
    Tree.maximum(testTree) should equal(10)

    Tree.maximum(Leaf(0)) should equal(0)
  }

  "Tree" should "find the depth of a tree correctly" in {
    Tree.depth(testTree) should equal(4)

    Tree.depth(Leaf("test")) should equal(1)
  }

}
