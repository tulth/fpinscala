package homework.parallelism.par

object Par {
}
object Main {
  def main(args: Array[String]): Unit = {
    println("Par!")

    // ex 7.1: Par.map2 is a new higher-order function for combining the result of two parallel computations.
    // What is its signature?
    // Give the most general signature possible (don’t assume it works only for Int).
    // def map2[A,B,C](pa: Par[A], pb: Par[B])(f: [A,B]=>C): Par[C]
  }
}
