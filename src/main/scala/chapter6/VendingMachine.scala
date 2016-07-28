package chapter6

/**
  * Created by paul on 7/28/16.
  */
case class VendingMachine(locked: Boolean, candies: Int, coins: Int)

abstract sealed trait Input
case object Coin extends Input
case object Turn extends Input


object VendingMachine {
  def simulateMachine(inputs: List[Input]): State[VendingMachine, (Int, Int)] = {
    ???
  }
}