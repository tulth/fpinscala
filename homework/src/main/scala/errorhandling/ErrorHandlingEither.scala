package homework.errorhandling.either

import scala.{Option => _, Either => _, _}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case Left(v) => Left(v)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(v) => Left(v)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(v) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(tv => b.map(bv => f(tv, bv)))

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object ErrorHandlingEither {
  def mean(xs: Seq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h::t => f(h).map2(traverse(t)(f))(_::_)
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  def main(args: Array[String]): Unit = {
    println("Error Handling Either!")
    println(mean(List(1.0,2.0,3.0)))
    println(mean(List()))

    // ex 4.6: Implement versions of map, flatMap, orElse, and map2 on Either that operate
    // on the Right value
    println(f"ex4.6: ${Right(1).map((_+1))}")
    println(f"ex4.6: ${Right(1).flatMap(x => Right(x+1))}")
    println(f"ex4.6: ${Right(1).orElse(Right(2))}")
    println(f"ex4.6: ${Left("Nope").orElse(Right(2))}")
    println(f"ex4.6: ${Right(1).map2(Right(2))((a:Int, b:Int) => a+b)}")
    println(f"ex4.6: ${Right(1).map2(Left("tfb"))((a:Int, b:Int) => a+b)}")

    // ex 4.7: Implement sequence and traverse for Either
    val stringList0 = List("1","2","3")
    val stringList1 = List("1","2","a")
    val stringList2 = List("no","2","no2")
    println(f"ex4.7: ${traverse(stringList0)(v => Try(v.toInt))}")
    println(f"ex4.7: ${traverse(stringList1)(v => Try(v.toInt))}")
    println(f"ex4.7: ${traverse(stringList2)(v => Try(v.toInt))}")
    println(f"ex4.7: ${sequence(List(Right(1),Right(2),Right(3)))}")
    println(f"ex4.7: ${sequence(List(Right(1),Right(2),Left("problem?")))}")
    println(f"ex4.7: ${sequence(List(Left("terribly wrong"),Right(2),Right(3)))}")
    println(f"ex4.7: ${sequence(List())}")

    // ex 4.8: In this implementation, map2 is only able to report one error, even if both the name
    // and the age are invalid. What would you need to change in order to report both errors?
    // Would you change map2 or the signature of mkPerson? Or could you create a new data
    // type that captures this requirement better than Either does, with some additional
    // structure? How would orElse, traverse, and sequence behave differently for that
    // data type?
    //
    // ans: I would not change map2.
    // I would be inclined to make an Either variant where all possible statements get executed
    // but errors are gathered as a list
  }
}
