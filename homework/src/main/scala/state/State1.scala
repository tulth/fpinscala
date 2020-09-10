package homework.state1

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

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (rngNum, nextState) = rng.nextInt
    (if (rngNum < 0) -(rngNum+1) else rngNum, nextState)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def double: Rand[Double] = map(nonNegativeInt)(n => n / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a,b), rng3)

  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft((rng:RNG) => (Nil:List[A], rng))((f, acc) => map2(f, acc)((a, b) => b :: a))
  // def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
  //   @annotation.tailrec
  //   def go(fs: List[Rand[A]], rs: List[A], rng: RNG): (List[A], RNG) = fs match {
  //     case Nil => (rs, rng)
  //     case h::t => {
  //       val (v, rng1) = h(rng)
  //       go(t, v::rs, rng1)
  //     }
  //   }
  //   rng0 => go(fs, Nil, rng0)
  // }

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def doubles(count: Int): Rand[List[Double]] =
    sequence(List.fill(count)(double))
    // {
    //   @annotation.tailrec
    //   def go(n: Int, rngArg: RNG, intermediateResult: List[Int]): (List[Int], RNG) = {
    //     if (n > 0) {
    //       val (ri, rngNext) = rngArg.nextInt
    //       go(n-1, rngNext, ri::intermediateResult)
    //     }
    //     else {
    //       (intermediateResult, rngArg)
    //     }
    //   }
    //   go(count, rng, Nil)
    // }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (value, nextRng) = f(rng)
    g(value)(nextRng)
  }

  def nonNegativeLessThan(min: Int): Rand[Int] =
    flatMap(nonNegativeInt)( i => {
      val mod = i % min
      if (i + (min-1) - mod >= 0)
        unit(mod)
      else nonNegativeLessThan(min)
    })

  def mapViaFlatMap[A,B](f: Rand[A])(g: A=>B): Rand[B] =
    flatMap(f)(a=>unit(g(a)))

  def map2ViaFlatMap[A,B,C](r0: Rand[A], r1: Rand[B])(f: (A,B)=>C): Rand[C] =
    flatMap(r0)(a => map(r1)(b => f(a,b)))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

  def main(args: Array[String]): Unit = {
    val rngGen = SimpleRNG(seed=42L)

    println("State part 1")
    println(f"int(rngGen): ${int(rngGen)}")
    println(f"nonNegativeInt(rngGen): ${nonNegativeInt(rngGen)}")
    println(f"map(int)(_+1)(rngGen): ${map(int)(_+1)(rngGen)}")
    println(f"map(map(int)(_+1))(_-1)(rngGen): ${map(map(int)(_+1))(_-1)(rngGen)}")
    println(f"nonNegativeEven(rngGen): ${nonNegativeEven(rngGen)}")

    // ex 6.5: Use map to reimplement double in a more elegant way. See exercise 6.2
    println(f"6.5: ${double(rngGen)}")

    // ex 6.6: Write the implementation of map2 based on the following signature.
    // This function takes two actions, ra and rb, and a function f for combining their
    // results, and returns a new action that combines them:
    println(f"6.6: ${map2(double, double)((a,b) => a*a+b*b)(rngGen)}")

    // ex 6.7: Hard: If you can combine two RNG transitions, you should be able to combine a whole
    // list of them.
    // Implement sequence for combining a List of transitions into a single transition.
    // Use it to reimplement the ints function you wrote before.
    // For the latter, you can use the standard library function List.fill(n)(x) to make
    // a list with x repeated n times.
    println(f"6.7: ${ints(5)(rngGen)}")
    println(f"6.7: ${ints(0)(rngGen)}")
    println(f"6.7: ${doubles(5)(rngGen)}")

    // ex 6.8: Implement flatMap, and then use it to implement nonNegativeLessThan.
    println(f"6.8: ${sequence(List.fill(10)(nonNegativeLessThan(5)))(rngGen)}")

    // ex 6.9:
    println(f"6.9: mapViaFlatMap(int)(_+1)(rngGen): ${map(int)(_+1)(rngGen)}")
    println(f"6.9: mapViaFlatMap(map(int)(_+1))(_-1)(rngGen): ${map(map(int)(_+1))(_-1)(rngGen)}")
    println(f"6.9: ${map2ViaFlatMap(double, double)((a,b) => a*a+b*b)(rngGen)}")
  }
}
