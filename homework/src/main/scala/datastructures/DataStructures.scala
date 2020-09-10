package homework.datastructures
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ints: List[Int]): Int = ints match {
    case Nil => 1
    case Cons(x,xs) => x * product(xs)
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) => Cons(h,xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) => if (n>0) drop(xs, n-1) else l
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) => if (f(x)) dropWhile(xs, f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x,Nil) => Nil
    case Cons(x,xs) => Cons(x, init(xs))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def length[A](l: List[A]): Int = foldRight(l, 0)((_:A, a:Int) => 1 + a)

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sumViaFoldLeft(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def productViaFoldLeft(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((a:Int, _:A) => 1 + a)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((b,a) => Cons(a,b))

  def foldLeftViaFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((a,b) => f(b,a))  // cheating because reverse uses foldLeft

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a,b))

  def appendViaFoldLeft[A](as: List[A], bs: List[A]): List[A] =
    foldLeft(reverse(as), bs)((b, a) => Cons(a,b))

  def concatViaFoldLeft[A](ll: List[List[A]]): List[A] =
    foldLeft(ll, Nil:List[A])(appendViaFoldLeft)

  def concatViaFoldRight[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(appendViaFoldLeft)

  def add1(l: List[Int]): List[Int] =
    // l match {
    //   case Nil => Nil
    //   case Cons(x, xs) => Cons(x+1, add1(xs))
    // }
    reverse(foldLeft(l, Nil:List[Int])((b,a) => Cons(1+a,b)))

  def mapToString(l: List[Double]): List[String] =
    reverse(foldLeft(l, Nil:List[String])((b,a) => Cons(a.toString,b)))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(as, Nil:List[B])((a,b) => Cons(f(a),b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(as, Nil:List[A])((a,b) => if (f(a)) Cons(a,b) else b)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    // foldRightViaFoldLeft(as, Nil:List[B])((a,b) => appendViaFoldLeft(f(a),b))
    concatViaFoldLeft(map(as)(f))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def addLists(as: List[Int], bs: List[Int]): List[Int] =
    (as, bs) match {
      case (Nil, _) => bs
      case (_, Nil) => as
      case (Cons(ah, at), Cons(bh, bt)) => Cons(ah+bh, addLists(at, bt))
    }

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B)=>C): List[C] =
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah,bh), zipWith(at, bt)(f))
    }

  def startsWith[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(ah, at), Cons(bh, bt)) => (ah==bh) && startsWith(at, bt)
    }

  def tails[A](arg: List[A]): List[List[A]] = arg match {
    case Nil => Nil
    case Cons(x,xs) => Cons(arg, tails(xs))
  }

  def any(arg: List[Boolean]): Boolean = arg match {
    case Nil => false
    case Cons(x,xs) => if (x) true else any(xs)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    any(map(tails(sup))(seq => startsWith(seq, sub)))

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail:_*))

}

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(lf: A=>B)(bf: (B,B) => B): B = t match {
    case Leaf(x) => lf(x)
    case Branch(l, r) => bf(fold(l)(lf)(bf), fold(r)(lf)(bf))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_=>1)((l, r) => 1 + l + r)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(x=>x)((l, r) => l max r)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_=>0)((l, r) => (l max r) + 1)

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(x=>Leaf(f(x)):Tree[B])((l, r) => Branch(l,r))

}

object DataStructures {
  def main(args: Array[String]): Unit = {
    println("Data Structures!")
    val nums = List(1,2,3,4)
    println("sum(nums)=%d".format(List.sum(nums)))
    println("product(nums)=%d".format(List.product(nums)))

    // ex 3.1:  I think the answer is 3
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) =>x+y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    println(f"exercise 3.1: result is ${x}")

    // ex 3.2: implement tail for removing the first element of a list
    val testList = List(1,2,3,4,5)
    println(f"3.2: ${List.tail(testList)}")

    // ex 3.3: implement setHead for replacing the first element of a list
    println(f"3.3: ${List.setHead(testList, 6)}")

    // ex 3.4: implement drop for removing the leading elements of a list
    println(f"3.4: ${List.drop(testList, 2)}")
    println(f"3.4: ${List.drop(testList, 0)}")

    // ex 3.5: implement dropWhile for removing the leading elements of a list
    println(f"3.5: ${List.dropWhile(testList, (x:Int) => x < 2)}")
    println(f"3.5: ${List.dropWhile(testList, (x:Int) => x < 10)}")
    // println(f"3.5: ${List.dropWhile(testList, x => x < 10)}")

    // ex 3.6: implement init for removing all but he last element
    println(f"3.6: ${List.init(testList)}")

    // ex 3.7: Can product, implemented using foldRight, immediately halt the recursion and
    //    return 0.0 if it encounters a 0.0? Why or why not?
    // No, because foldright calls fold right recursively for the input argument to function f;
    //  f has no option to exit early.

    // ex 3.8: pass nil and cons to foldright
    println(f"3.8: ${List.foldRight(testList, Nil:List[Int])(Cons(_,_))}")
    // we get the same list back.
    // This suggests foldRight can be used to implement the list data constructor

    // ex 3.9: Compute the length of a list using foldRight
    println(f"3.9: ${List.length(testList)}")

    // ex 3.10: foldLeft
    println(f"3.10: ${List.foldLeft(testList, 1)(_*_)}")
    println(f"3.10: ${List.foldLeft(testList, 0)(_+_)}")
    println(f"3.10: ${List.foldLeft(testList, 0)(_-_)}")
    println(f"3.10: ${List.foldRight(testList, 0)(_-_)}")  // foldRight is different on -

    // ex 3.11: foldLeft
    println(f"3.11: ${List.sumViaFoldLeft(testList)}")
    val testList2 = List(1.0,2.0,3,4,5)
    println(f"3.11: ${List.productViaFoldLeft(testList2)}")
    println(f"3.11: ${List.lengthViaFoldLeft(testList2)}")

    // ex 3.12: reverse
    println(f"3.12: ${List.reverse(testList)}")

    // ex 3.13a: Hard: Can you write foldLeft in terms of foldRight?
    println(f"3.13a: ${List.foldLeftViaFoldRight(testList, 1)(_*_)}")
    println(f"3.13a: ${List.foldLeftViaFoldRight(testList, 0)(_+_)}")
    println(f"3.13a: ${List.foldLeft(testList, 0)(_-_)} ${List.foldLeftViaFoldRight(testList, 0)(_-_)} ")
    println(f"3.13a: ${List.foldLeftViaFoldRight(testList, Nil:List[Int])((b,a) => Cons(a,b))}")

    // ex 3.13b: Hard: Can you write foldRight in terms of foldLeft ?
    println(f"3.13b: ${List.foldRightViaFoldLeft(testList, 1)(_*_)}")
    println(f"3.13b: ${List.foldRightViaFoldLeft(testList, 0)(_+_)}")
    println(f"3.13b: ${List.foldRight(testList, 0)(_-_)} ${List.foldRightViaFoldLeft(testList, 0)(_-_)} ")
    println(f"3.13b: ${List.foldRightViaFoldLeft(testList, Nil:List[Int])(Cons(_,_))}")

    // ex 3.14: append
    println(f"3.14: ${List.appendViaFoldLeft(testList, testList)}")

    // ex 3.15: Hard: Write a function that concatenates a list of lists into a single list.
    // Its runtime should be linear in the total length of all
    // lists. Try to use functions we ahve already defined.
    println(f"3.15: ${List.concatViaFoldLeft(List(testList, List.reverse(testList), List(6,7,8,9)))}")
    println(f"3.15: ${List.concatViaFoldRight(List(testList, List.reverse(testList), List(6,7,8,9)))}")

    // ex 3.16: Write a function that transforms a list of integers by adding 1 to each element.
    // (Reminder: this should be a pure function that returns a new List!)
    println(f"3.16: ${List.add1(testList)}")

    // ex 3.17: Write a function that turns each value in a List[Double] into a String.
    // You can use the expression d.toString to convert some d: Double to a String.
    println(f"3.17: ${List.mapToString(testList2)}")

    // ex 3.18: Write a function map that generalizes modifying each element in a list while
    // maintaining the structure of the list.
    println(f"3.18: ${List.map(testList)(_+10)}")

    // ex 3.19: Write a function filter that removes elements from a list unless they satisfy
    // a given predicate. Use it to remove all odd numbers from a List[Int].
    println(f"3.19: ${List.filter(testList)(_ % 2 == 0)}")

    // ex 3.20: Write a function flatMap that works like map except that the function given will
    // return a list instead of a single result, and that list should be inserted into the final
    // resulting list.
    // For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in
    // List(1,1,2,2,3,3).
    println(f"3.20: ${List.flatMap(testList)(i => List(i,i))}")
    // compare to map println(f"3.20: ${List.map(testList)(i => List(i,i))}")

    // ex 3.21: Use flatMap to implement filter.
    println(f"3.21: ${List.filterViaFlatMap(testList)(_ % 2 == 0)}")

    // ex 3.22: Write a function that accepts two lists and constructs a new list by adding
    // corresponding elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
    println(f"3.22: ${List.addLists(List(1,2,3), List(4,5,6))}")

    // ex 3.23: Generalize the function you just wrote so that it’s not specific to integers or addition.
    // Name your generalized function zipWith.
    println(f"3.23: ${List.zipWith(List(1,2,3), List(4,5,6))(_+_)}")

    // ex 3.24: Hard: As an example, implement hasSubsequence for checking whether a List
    // contains another List as a subsequence.
    // For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences, among others.
    // You may have some difficulty finding a concise purely functional implementation that is also efficient.
    // That’s okay. Implement the function however comes most naturally.
    // We’ll return to this implementation in chapter 5 and hopefully improve on it.
    // Note: Any two values x and y can be compared for equality in Scala using the expression x == y.
    println(f"3.24: ${List.tails(List(1,2,3,4))}")
    println(f"3.24: ${List.hasSubsequence(List(1,2,3,4), List(1,2))}")
    println(f"3.24: ${List.hasSubsequence(List(1,2,3,4), List(2,3))}")
    println(f"3.24: ${List.hasSubsequence(List(1,2,3,4), List(4))}")
    println(f"3.24: ${List.hasSubsequence(List(1,2,3,4), List(3,2))}")

    // ex 3.25: Write a function size that counts the number of nodes (leaves and branches) in a tree
    val testTree0 = Leaf(42)
    val testTree1 = Branch(Leaf(12), Branch(Leaf(8), Leaf(4)))
    println(f"3.25: ${Tree.size(testTree0)}")
    println(f"3.25: ${Tree.size(testTree1)}")

    // ex 3.26: Write a function size that counts the number of nodes (leaves and branches) in a tree
    val testTree2 = Branch(Branch(Leaf(12), Leaf(77)), Branch(Leaf(8), Leaf(4)))
    println(f"3.26: ${Tree.maximum(testTree0)}")
    println(f"3.26: ${Tree.maximum(testTree1)}")
    println(f"3.26: ${Tree.maximum(testTree2)}")

    // ex 3.27: Write a function depth that returns the maximum path length from the root of a tree to any leaf.
    println(f"3.27: ${Tree.depth(testTree0)}")
    println(f"3.27: ${Tree.depth(testTree1)}")
    println(f"3.27: ${Tree.depth(testTree2)}")

    // ex 3.28: Write a function map, analogous to the method of the same name on List, that
    // modifies each element in a tree with a given function
    println(f"3.28: ${Tree.map(testTree0)(_+1)}")
    println(f"3.28: ${Tree.map(testTree1)(_+1)}")
    println(f"3.28: ${Tree.map(testTree2)(_+1)}")

    // ex 3.29: Generalize size, maximum, depth, and map, writing a new function fold
    // that abstracts over their similarities.
    // Reimplement them in terms of this more general function.
    println(f"3.29: ${Tree.sizeViaFold(testTree0)}")
    println(f"3.29: ${Tree.sizeViaFold(testTree1)}")
    println(f"3.29: ${Tree.maximumViaFold(testTree0)}")
    println(f"3.29: ${Tree.maximumViaFold(testTree1)}")
    println(f"3.29: ${Tree.maximumViaFold(testTree2)}")
    println(f"3.29: ${Tree.depthViaFold(testTree0)}")
    println(f"3.29: ${Tree.depthViaFold(testTree1)}")
    println(f"3.29: ${Tree.depthViaFold(testTree2)}")
    println(f"3.29: ${Tree.mapViaFold(testTree0)(_+1)}")
    println(f"3.29: ${Tree.mapViaFold(testTree1)(_+1)}")
    println(f"3.29: ${Tree.mapViaFold(testTree2)(_+1)}")

  }
}
