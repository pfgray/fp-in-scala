package chapter10

import chapter5.Stream
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author pgray
  */
class FoldableTest  extends FlatSpec with Matchers {


  "Foldable" should "convert Foldables to List" in {
    val stream = Stream(1, 2, 3, 4, 5)

    val res = Foldable.foldableStream.toList(stream)

    res should be(List(1, 2, 3, 4, 5))
  }

  "Foldable" should "create bags with monoids" in {
    val stream = IndexedSeq("one", "two", "two", "three", "three", "three")

    val res = Foldable.bag(stream)


    res should be(Map(
      "one" -> 1,
      "two" -> 2,
      "three" -> 3
    ))
  }

}
