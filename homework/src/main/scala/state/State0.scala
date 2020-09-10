package homework.state0

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}

object Main {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (rngNum, nextState) = rng.nextInt
    (if (rngNum < 0) -(rngNum+1) else rngNum, nextState)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (rngNatural, nextState) = nonNegativeInt(rng)
    val rngDouble = (rngNatural / (Int.MaxValue.toDouble+1))
    (rngDouble, nextState)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (rngInt, s0) = rng.nextInt
    val (rngDouble, s1) = double(s0)
    ((rngInt, rngDouble), s1)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((rngInt, rngDouble), nxtRng) = intDouble(rng)
    ((rngDouble, rngInt), nxtRng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val ((rngDouble0), nxtRng0) = double(rng)
    val ((rngDouble1), nxtRng1) = double(nxtRng0)
    val ((rngDouble2), nxtRng2) = double(nxtRng1)
    ((rngDouble0, rngDouble1, rngDouble2), nxtRng2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(n: Int, rngArg: RNG, intermediateResult: List[Int]): (List[Int], RNG) = {
      if (n > 0) {
        val (ri, rngNext) = rngArg.nextInt
        go(n-1, rngNext, ri::intermediateResult)
      }
      else {
        (intermediateResult, rngArg)
      }
    }
    go(count, rng, Nil)
  }

  def main(args: Array[String]): Unit ={
    println("State part 0")
    val rngGen = SimpleRNG(seed=75992012648784L)
    println(f"${rngGen.nextInt}")

    // ex 6.1: Write a function that uses RNG.nextInt to generate a random integer between 0 and
    // Int.maxValue (inclusive).
    // Make sure to handle the corner case when nextInt returns Int.MinValue, which
    // doesn’t have a non-negative counterpart.
    println(f"6.1: ${nonNegativeInt(rngGen)}")

    // ex 6.2: Write a function to generate a Double between 0 and 1, not including 1.
    // Note: You can use Int.MaxValue to obtain the maximum positive integer value, and
    // you can use x.toDouble to convert an x: Int to a Double.
    println(f"6.2: ${double(rngGen)}")

    // ex 6.3: Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
    // (Double, Double, Double) 3-tuple.
    // You should be able to reuse the functions you’ve already written.
    println(f"6.3: ${intDouble(rngGen)}")
    println(f"6.3: ${doubleInt(rngGen)}")
    println(f"6.3: ${double3(rngGen)}")

    // ex 6.4: Write a function to generate a list of random integers.
    println(f"6.4: ${ints(5)(rngGen)}")
    println(f"6.4: ${ints(0)(rngGen)}")
  }
}
