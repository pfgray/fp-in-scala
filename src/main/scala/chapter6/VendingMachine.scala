package chapter6

import State._

/**
  * Created by paul on 7/28/16.
  */
case class VendingMachine(locked: Boolean, candies: Int, coins: Int)

abstract sealed trait Input
case object Coin extends Input
case object Turn extends Input

object VendingMachine {

  def simulateMachine(inputs: List[Input]): State[VendingMachine, (Int, Int)] =
    State(machine => {
      inputs.foldLeft(toTup(machine)) { (s, input) =>
        val processed = processInput(input).run(s._2)
        println(s"Processed $input, Now we have machine: ${processed._2}")
        processed
      }
    })

  private def processInput(input: Input): State[VendingMachine, (Int, Int)] =
    State(machine => {
      input match {
        case Coin => processCoin(machine)
        case Turn => processTurn(machine)
      }
    })

  private def processCoin(m: VendingMachine) =
    if(m.candies > 0){
      toTup(m.copy(locked = false, coins = m.coins + 1))
    } else {
      toTup(m)
    }

  private def processTurn(m: VendingMachine) =
    if(m.candies > 0 && !m.locked){
      toTup(m.copy(candies = m.candies - 1, locked = true))
    } else {
      toTup(m)
    }

  private def toTup(machine: VendingMachine): ((Int, Int), VendingMachine) = {
    ((machine.candies, machine.coins), machine)
  }
}