package state

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  private[this] val rule: (Machine, Input) => Machine = (machine, input) => {
    input match {
      case Coin if machine.locked && machine.candies > 0 =>
        machine.copy(locked = false, coins = machine.coins + 1)
      case Turn if machine.locked == false =>
        machine.copy(locked = true, candies = machine.candies - 1)
      case _ => machine
    }
  }

  def simulateMachine(input: Input): State[Machine, (Int, Int)] = {
    val run: Machine => ((Int, Int), Machine) = machine => {
      val nextMachine = rule(machine, input)
      ((nextMachine.coins, nextMachine.candies), nextMachine)
    }
    State(run)
  }

  // Exercise 6.11
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val run: Machine => ((Int, Int), Machine) = machine => {
      val lastMachine = inputs.foldLeft(machine) { (m, in) =>
        val (_, nextMachine) = simulateMachine(in).run(m)
        nextMachine
      }
      ((lastMachine.coins, lastMachine.candies), lastMachine)
    }
    State(run)
  }

}
