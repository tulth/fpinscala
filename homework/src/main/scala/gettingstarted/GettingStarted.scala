object GettingStarted {
  // def abs(n: Int): Int =
  def abs(n: Int) =
    if (n<0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  // def fib(num: Int): Int =
  //   if (num == 0) 0
  //   else if (num == 1) 1
  //   else fib(num-1) + fib(num-2)
  // def fib(num: Int): Int = {
  //     if (num <= 0) 0
  //     else if (num == 1) 1
  //     else {
  //       def loop(num: Int, prev: Int, prevPrev: Int): Int =
  //         if (num < 2) prev+prevPrev 
  //         else
  //           loop(num-1, prev+prevPrev, prev)
  //       loop(num-1, 1, 0)
  //     }
  // }

  def fib(num: Int): Int = {
    @annotation.tailrec
    def loop(num: Int, prev: Int, prevPrev: Int): Int =
      if (num == 0) prevPrev
      else if (num == 1) prev
      else
        loop(num-1, prev+prevPrev, prev)
    loop(num, 1, 0)
  }

  private def fibTest(fibNum: Int, expect: Int) = {
    val msg = "The %dth fib did not match expect: %d actual: %d"
    val actual = fib(fibNum)
    assert(expect == actual, msg.format(fibNum, expect, actual))
  }

  private def fibTestAll() = {
    fibTest(0,0)
    fibTest(1,1)
    fibTest(2,1)
    fibTest(3,2)
    fibTest(4,3)
    fibTest(5,5)
    fibTest(6,8)
    println("fibTestAll: All tests passed")
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(idx: Int): Boolean = {
      if (idx >= as.length) true
      else if (ordered(as(idx-1), as(idx))) loop(idx+1)
      else false
    }
    loop(1)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def main(args: Array[String]): Unit = {
    println("Getting Started!")
    println(formatAbs(-42))
    fibTestAll()

    val testArr0 = Array(0, 2, 3, 4)
    def lt(x:Int,y:Int):Boolean = x < y
    if (isSorted(testArr0, lt))
      println("testArr0 is sorted")
    else
      println("testArr0 is NOT sorted")

    val testArr1 = Array(0, 2, 99, 4)
    if (isSorted(testArr1, lt))
      println("testArr1 is sorted")
    else
      println("testArr1 is NOT sorted")
  }
}
