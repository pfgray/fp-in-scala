package chapter2

import org.scalatest.{FlatSpec, Matchers}

class MyModuleTest extends FlatSpec with Matchers {
  "MyModule" should "return the nth fib number" in {
    MyModule.fib(5) should equal(3)
    MyModule.fib(1) should equal(0)
    MyModule.fib(20) should equal(4181)
  }

  it should "detect sorted arrays correctly" in {

    MyModule.isSorted[Int](Array(1, 2, 3, 4, 5, 6), _ < _) should equal(true)

    MyModule.isSorted[Int](Array(1, 2, 3, 8, 4, 5, 6), _ < _) should equal(false)
  }

  it should "curry uncurried functions" in {

    val add: (Int, Int) => Int = _ + _

    val curriedAdd = MyModule.curry(add)

    curriedAdd(3)(5) should equal(8)

  }

  it should "uncurry curried functions" in {

    val curriedAdd: Int => Int => Int = a => b => a + b

    val add = MyModule.uncurry(curriedAdd)

    add(7, 16) should equal(23)

  }

  it should "compose functions" in {

    val addThree: Int => Int = a => a + 3
    val multiplyByTwo: Int => Int = a => a * 2

    val addThreeThenTimesTwo = MyModule.compose(multiplyByTwo, addThree)

    addThreeThenTimesTwo(17) should equal(40)

    val timesTwoThenAddThree = MyModule.compose(addThree, multiplyByTwo)

    timesTwoThenAddThree(17) should equal(37)

  }
}
