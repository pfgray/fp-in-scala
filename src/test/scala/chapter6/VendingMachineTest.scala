package chapter6

import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by paul on 7/31/16.
  */
class VendingMachineTest extends FlatSpec with Matchers {

  "VendingMachine" should "work correctly" in {
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn)

    val ((candies, coins), machine) = VendingMachine.simulateMachine(inputs).run(VendingMachine(true, 20, 0))

    coins should equal(3)
    candies should equal(17)
  }

  it should "work correctly again" in {
    val inputs = List(Turn, Turn, Turn, Turn, Coin, Coin, Turn)

    val ((candies, coins), machine) = VendingMachine.simulateMachine(inputs).run(VendingMachine(true, 20, 0))

    coins should equal(2)
    candies should equal(19)
  }

}
