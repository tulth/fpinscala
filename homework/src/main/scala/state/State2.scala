package homework.state2

import homework.state1.{RNG, SimpleRNG}

case class State[S,+A](run: S => (A,S)) {
  def flatMap[B](f: A => State[S,B]): State[S,B] =
    State(s0 => {
      val (valueA, s1) = this.run(s0)
      f(valueA).run(s1)
    })

  def map[B](f: A=>B): State[S,B] =
    this.flatMap(a => State.unit(f(a)))

  def map2[B,C](other: State[S, B])(f: (A,B)=>C): State[S,C] =
    this.flatMap(a => other.map(b => f(a,b)))

  def getState: State[S, S] =
    State(s => this.run(s) match {
      case (a, s1) => (s1, s1)
    })
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def traverse[S,A,B](arg: List[A])(f: A => State[S, B]): State[S, List[B]] =
    arg.reverse.foldLeft(unit[S,List[B]](Nil:List[B]))((sl, s) =>
      f(s).map2(sl)(_::_)
    )

  def sequence[S,A](arg: List[State[S, A]]): State[S, List[A]] =
    traverse(arg)(x => x)
    // arg.reverse.foldLeft(unit[S,List[A]](Nil:List[A]))((sl, s) =>
    //   s.map2(sl)(_::_)
    // )

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def insertCoin: Machine = if (candies > 0) Machine(false, candies, coins + 1) else this

  def turnKnob: Machine = if (!locked && (candies > 0)) Machine(true, candies-1, coins) else this

  def updateMachine(input: Input): Machine = {
    println(f"updateMachine this=${this} input=${input}")
    input match {
    case Coin if this.candies > 0 => this.insertCoin
    case Turn if this.candies > 0 => this.turnKnob
    case _ => this
  }}

}

object Main {
  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = State((a:RNG) => a.nextInt)

  def nonNegativeInt: Rand[Int] = int.map(rngNum => {
    if (rngNum < 0) -(rngNum+1) else rngNum
  })

  def nonNegativeLessThan(min: Int): Rand[Int] =
    nonNegativeInt.flatMap( i => {
      val mod = i % min
      if (i + (min-1) - mod >= 0)
        State.unit(mod)
      else nonNegativeLessThan(min)
    })

  def updateMachine(input: Input): State[Machine, (Int, Int)] =
    State((m:Machine) => {
      val um = m.updateMachine(input)
      ((um.coins, um.candies), um)
    })

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val stateSeq = State.sequence(inputs.map(updateMachine))
    def getResult(m: Machine): ((Int, Int), Machine) = ((m.coins, m.candies), m)
    State(s => {
      val ast = stateSeq.run(s)
      val m = ast._2
      getResult(m)})
  }

  def main(args: Array[String]): Unit = {
    println("State part 2")
    val rngGen = SimpleRNG(seed=42L)
    println(f"${int.run(rngGen)})")
    println(f"nonNegativeInt.run(rngGen): ${nonNegativeInt.run(rngGen)}")

    // ex 6.10: Generalize the functions unit, map, map2, flatMap, and sequence.
    // Add them as methods on the State case class where possible.
    // Otherwise you should put them in a State companion object.
    println(f"6.10: nonNegativeLessThan using flatMap ${nonNegativeLessThan(5).run(rngGen)}")
    println(f"6.10: map ${int.map(_+1).run(rngGen)}")
    println(f"6.10: map2 ${int.map2(int)((a,b) => a*a+b*b).run(rngGen)}")
    println(f"6.10: ${State.sequence(List.fill(10)(nonNegativeLessThan(5))).run(rngGen)}")

    // ex 6.11: Hard: To gain experience with the use of State, implement a finite
    // state automaton that models a simple candy dispenser.
    // The machine has two types of input: you can insert a coin, or
    // you can turn the knob to dispense candy.
    // It can be in one of two states: locked or unlocked.
    // It also tracks how many candies are left and how many coins it contains.
    val candyBox = Machine(true, 5, 10)
    val testInputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val testInputsMT = List()
    println(f"6.11: ${updateMachine(Coin).run(candyBox)}")
    println(f"6.11: ${testInputs.map(updateMachine).head.run(candyBox)}")
    println(f"6.11: ${simulateMachine(testInputs).run(candyBox)}")
    println(f"6.11: ${simulateMachine(testInputsMT).run(candyBox)}")
  }
}
