package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def appendUsingFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    // foldRight(a1, a2)(Cons(_,_))
    foldLeft(reverse(a1), a2)((as, a) => Cons(a, as))
  
  def concat[A](ls: List[List[A]]): List[A] =
    foldRightUsingFoldLeft(ls, Nil:List[A])(append(_,_))
  
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(a, as) =>
      foldLeft(as, f(z, a))(f)
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((as: List[A], a:A) => Cons(a,as))


  def foldLeftUsingFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((b, a) => f(a,b)) // book answer said this was cheating

  def foldRightUsingFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((a, b) => f(b,a))
  
  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar
  
  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n - 1)
    }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) dropWhile(xs, f)
      else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, x:Int)=>1+x)
  
  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((x:Int, _)=>1+x)
  
  def map[A,B](l: List[A])(f: A => B): List[B] = ???

  def main(args: Array[String]): Unit = {
    println("Data Structures List!")
    val l0 = List(1,2,3)
    println(f"l0: $l0")
    println(f"ex3.2 tail(l0): ${tail(l0)}")
    println(f"ex3.3 setHead(l0, 0): ${setHead(l0, 0)}")
    println(f"ex3.4 drop(l0, 1): ${drop(l0, 1)} drop(l0, 2): ${drop(l0, 2)}")
    val l1 = List(1,2,3,7,5,6)
    println(f"l1: $l1")
    println(f"ex3.5 dropWhile(l1, x:Int=>x<4): ${dropWhile(l1, (x:Int)=>x<4)}")
    println(f"ex3.6 init(l0): ${init(l0)} init(l1): ${init(l1)}")
    // ex3.7: product always calls itself and it will not stop short
    // ex3.8
    // I think it is identity-like, returns the same data
    // This suggests data constructors are an application of foldRight
    println(f"ex3.8 foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) = List(1,2,3) = ${foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))}")
    println(f"ex3.9 length(l0): ${length(l0)} length(l1): ${length(l1)}")
    val l2 = List(1.1,2.2,3.3,7.7,5.5,6.6)
    println(f"ex3.10 sum2(l0): ${sum2(l0)} sum2(l1): ${sum2(l1)} sum3(l0): ${sum3(l0)} sum3(l1): ${sum3(l1)}")
    println(f"ex3.10 product2(l2): ${product2(l2)} product3(l2): ${product3(l2)}")
    println(f"ex3.10 length(l0): ${length(l0)} length(l1): ${length(l1)} length2(l0): ${length2(l0)} length2(l1): ${length2(l1)}")
    println(f"ex3.12 reverse(l0): ${reverse(l0)} reverse(l1): ${reverse(l1)}")
    println(f"ex3.13 see above, used reverse")
    println(f"ex3.14 append(l0, l1):              ${append(l0, l1)}")
    println(f"ex3.14 appendUsingFoldLeft(l0, l1): ${appendUsingFoldLeft(l0, l1)}")
    println(f"ex3.14 append(l1, l0):              ${append(l1, l0)}")
    println(f"ex3.14 appendUsingFoldLeft(l1, l0): ${appendUsingFoldLeft(l1, l0)}")
    val ls0 = List(l0, l1, List(9,7,8))
    println(f"ls0: $ls0")
    println(f"ex3.14 concat(ls0): ${concat(ls0)}")
  }
}
