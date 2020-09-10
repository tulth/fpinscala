package homework.errorhandling.option

import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(x) => f(x)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(x) => if (f(x)) this else None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object ErrorHandlingOption {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs.map(x => x*x)).flatMap(meanSqrs =>
      mean(xs).map(x => x*x).map(sqrMean =>
        meanSqrs - sqrMean))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(av => b.map(bv => f(av, bv)))

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    // a match {
    //   case Nil => Some(Nil)
    //   case h::t => map2(h, sequence(t))(_::_)
    // }
    a.foldRight[Option[List[A]]](Some(Nil:List[A]))((h,t) => map2(h, t)(_::_))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    // sequence(a map f)  // traverses 2x, no good
    a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverse(t)(f))(_::_)
    }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def main(args: Array[String]): Unit = {
    println("Error Handling Option!")
    println(mean(List(1,2,3)))
    println(mean(List()))

    // ex 4.1: define map, flatMap, getOrElse, orElse and filter on option
    println(f"ex4.1: ${Some(1).map((_+1))}")
    println(f"ex4.1: ${Some(1).flatMap(x => Some(x+1))}")
    println(f"ex4.1: ${Some(1).getOrElse(2)}")
    println(f"ex4.1: ${None.getOrElse(2)}")
    println(f"ex4.1: ${Some(1).orElse(Some(2))}")
    println(f"ex4.1: ${None.orElse(Some(2))}")
    println(f"ex4.1: ${Some(1).filter(x => x % 2 == 0)}")
    println(f"ex4.1: ${Some(2).filter(x => x % 2 == 0)}")

    // ex 4.2: Implement the variance function in terms of flatMap. If the mean of a sequence is m,
    // the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
    /// See the definition of variance on Wikipedia (http://mng.bz/0Qsr).
    println(f"ex4.2: ${variance(List(1,1,1))}")
    println(f"ex4.2: ${variance(List())}")
    println(f"ex4.2: ${variance(List(1,2,3))}")

    // ex 4.3: Write a generic function map2 that combines two Option values using a binary function.
    // If either Option value is None, then the return value is too.
    println(f"ex4.3: ${map2(Some(1), Some(2))((a:Int, b:Int) => a+b)}")
    println(f"ex4.3: ${map2(Some(1), None)((a:Int, b:Int) => a+b)}")
    println(f"ex4.3: ${map2(None, Some(2))((a:Int, b:Int) => a+b)}")

    // ex 4.4: Write a function sequence that combines a list of Options into one Option containing
    // a list of all the Some values in the original list.
    // If the original list contains None even once, the result of the function should be None.
    // Otherwise the result should be Some with a list of all the values.
    println(f"ex4.4: ${sequence(List(Some(1),Some(2),Some(3)))}")
    println(f"ex4.4: ${sequence(List(Some(1),Some(2),None))}")
    println(f"ex4.4: ${sequence(List(None,Some(2),Some(3)))}")
    println(f"ex4.4: ${sequence(List())}")

    // ex 4.5: Implement this traverse.
    // It’s straightforward to do using map and sequence, but try
    // for a more efficient implementation that only looks at the list once.
    // In fact, implement sequence in terms of traverse
    val stringList0 = List("1","2","3")
    val stringList1 = List("1","2","a")
    println(f"ex4.5: ${traverse(stringList0)(v => Try(v.toInt))}")
    println(f"ex4.5: ${traverse(stringList1)(v => Try(v.toInt))}")
    println(f"ex4.5: ${sequenceViaTraverse(List(Some(1),Some(2),Some(3)))}")
    println(f"ex4.5: ${sequenceViaTraverse(List(Some(1),Some(2),None))}")
    println(f"ex4.5: ${sequenceViaTraverse(List(None,Some(2),Some(3)))}")
    println(f"ex4.5: ${sequenceViaTraverse(List())}")
  }
}
