package homework.laziness

import scala.{Stream => _}

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], w: List[A]): List[A] = s match {
      case Empty => w
      case Cons(h, t) => go(t(), h()::w)// h()::(t().toList)
    }
    go(this, Nil:List[A]).reverse
  }

  def take(num:Int): Stream[A] =  this match {
    case Empty => Empty
    case Cons(h, t) if num>0 => Cons(h, ()=>t().take(num-1))
    case _ => Stream.empty
  }

  @annotation.tailrec
  final def drop(num:Integer): Stream[A] = this match {
    case Cons(h, t) if num>0 => t().drop(num-1)
    case _ => this
  }

  def takeWhile(p: A=>Boolean): Stream[A] =  this match {
    case Cons(h, t) if p(h()) => Cons(h, ()=>t().takeWhile(p))
    case _ => Stream.empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A=>Boolean): Stream[A] =
    this.foldRight(Stream.empty[A])((a,b) =>
      if (p(a))
        Stream.cons(a,b)
      else
        Stream.empty[A])

  def headOptionViaFoldRight: Option[A] =
    foldRight(None:Option[A])((a, _) => Some(a))

  def map[B](f: A=>B): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a),b))

  def filter(p: A=>Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a,b) else b)

  def append[B>:A](other: => Stream[B]): Stream[B] =
    foldRight(other)((a, b) => Stream.cons(a,b))

  def flatMap[B](f: A=>Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => f(a).append(b))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def mapViaUnfold[B](f: A=>B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold[B](num: Int): Stream[A] =
    Stream.unfold((this, num))
      { case (s, n) => s match {
        case Cons(h, t) if n>0 => Some((h(), (t(), n-1)))
        case _ => None
      }}

  def takeWhileViaUnfold(p: A=>Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h,t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B,C](bs: Stream[B])(f: (A,B)=>C): Stream[C] =
    Stream.unfold((this, bs))(ab => ab match {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    })

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    Stream.unfold((this, s2))(ab => ab match {
      case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case _ => None
    })

  def startsWith[A](s: Stream[A]): Boolean =  zipAll(s) forAll({
      case (_, None) => true
      case (None, _) => false
      case (Some(a), Some(b)) => (a==b)
  })

  def tails: Stream[Stream[A]] =
    Stream.unfold(this)(x => x match {
      case Cons(h,t) => Some((x, t()))
      case _ => None
    })

  def scanRight[B](z: B)(f: (A,=>B)=>B): Stream[B] =
    // tails.map(x => x.foldRight(z)(f)) // inefficient
    this match {
      case Empty => Stream(z)
      case Cons(h, t) => {
        lazy val next = t().scanRight(z)(f)
        next match {
          case Cons(nh, _) => Stream(f(h(), nh())).append(next)
        }
      }
    }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    // Stream.cons(a, constant(a))
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(a: Int): Stream[Int] = {
    Stream.cons(a, from(a+1))
  }

  def fibs: Stream[Int] = {
    // def go(prev:Int, curr:Int): Stream[Int] = Stream.cons(prev+curr, go(curr, prev+curr))
    // Stream.cons(0, Stream.cons(1, go(0, 1)))
    def go(prev:Int, curr:Int): Stream[Int] = Stream.cons(prev, go(curr, prev+curr))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Stream.empty
      case Some((value, nextState)) =>  Stream.cons(value, unfold(nextState)(f))
    }
  }

  val onesViaUnfold = unfold(Unit)(_ => Some((1, Unit)))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(Unit)(_ => Some((a, Unit)))

  def fromViaUnfold(a: Int): Stream[Int] = unfold(a)(x => Some((x, x+1)))

  def fibsViaUnfold: Stream[Int] = unfold((0,1))(
    { case (s0, s1) => Some(s0, (s1, s0+s1)) }
  )
  //((s0:Int, s1:Int)) => Some(s0, (s1, s0+s1)))

}

object Main {
  val ones: Stream[Int] = Stream.cons(1, ones)

  def main(args: Array[String]): Unit = {
    println("Laziness!")

    // ex 5.1: Implement Stream toList
    val testStream = Stream(1,2,3,4,5)
    println(f"5.1: ${testStream} ${testStream.toList}")

    // ex 5.2: Implement Stream take and drop
    println(f"5.2: ${testStream.take(2).toList}")
    println(f"5.2: ${testStream.take(0).toList}")
    println(f"5.2: ${testStream.take(10).toList}")
    println(f"5.2: ${testStream.drop(2).toList}")
    println(f"5.2: ${testStream.drop(0).toList}")
    println(f"5.2: ${testStream.drop(10).toList}")

    // ex 5.3: Implement Stream takeWhile
    println(f"5.3: ${testStream.takeWhile(_<3).toList}")
    println(f"5.3: ${testStream.takeWhile(_<0).toList}")
    println(f"5.3: ${testStream.takeWhile(_<10).toList}")

    // ex 5.4: Implement Stream forAll
    println(f"5.4: ${testStream.forAll(_<3)}")
    println(f"5.4: ${testStream.forAll(_<10)}")

    // ex 5.5: Implement Stream takeWhileViaFoldRight
    println(f"5.5: ${testStream.takeWhileViaFoldRight(_<3).toList}")
    println(f"5.5: ${testStream.takeWhileViaFoldRight(_<0).toList}")
    println(f"5.5: ${testStream.takeWhileViaFoldRight(_<10).toList}")

    // ex 5.6: Implement Stream headOptionViaFoldRight
    println(f"5.6: ${testStream.takeWhile(_<3).headOptionViaFoldRight}")
    println(f"5.6: ${testStream.takeWhile(_<0).headOptionViaFoldRight}")

    // ex 5.7: Implement Stream map, filter, append, and flatMap using foldRight.
    // The append method should be non-strict in its argument
    println(f"5.7: ${testStream.map(_+1).toList}")
    println(f"5.7: ${testStream.filter(_<3).toList}")
    println(f"5.7: ${testStream.filter(_>3).toList}")
    println(f"5.7: ${testStream.append(testStream).toList}")

    // ex 5.8: Generalize ones slightly to the function constant, which returns an infinite Stream of
    // a given value.
    val twos: Stream[Int] = Stream.constant(2)
    println(f"5.8: ${ones.take(5).toList}")
    println(f"5.8: ${twos.take(5).toList}")

    // ex 5.9: Write a function that generates an infinite stream of integers,
    // starting from n, then n + 1, n + 2, and so on
    println(f"5.9: ${Stream.from(1).take(5).toList}")
    println(f"5.9: ${Stream.from(10).take(5).toList}")

    // ex 5.10: Write a function fibs that generates the infinite stream of Fibonacci numbers:
    // 0, 1, 1, 2, 3, 5, 8, and so on
    println(f"5.10: ${Stream.fibs.take(2).toList}")
    println(f"5.10: ${Stream.fibs.take(10).toList}")

    // ex 5.11: Write a more general stream-building function called unfold.
    // It takes:
    //   - an initial state, and
    //   - a function for producing both the next state and the next value in the generated stream.
    println(f"5.11: ${Stream.unfold(Unit)(_ => Some((1, Unit))).take(5).toList}")

    // ex 5.12: Write fibs, from, constant, and ones in terms of unfold
    println(f"5.12: ${Stream.onesViaUnfold.take(5).toList}")
    println(f"5.12: ${Stream.constantViaUnfold(2).take(5).toList}")
    println(f"5.12: ${Stream.fromViaUnfold(1).take(5).toList}")
    println(f"5.12: ${Stream.fromViaUnfold(10).take(5).toList}")
    println(f"5.12: ${Stream.fibsViaUnfold.take(10).toList}")

    // ex 5.13: Use unfold to implement
    // map, take, takeWhile, zipWith (as in chapter 3), and zipAll.
    // The zipAll function should continue the traversal as long as either stream
    // has more elements—it uses Option to indicate whether each stream has been
    // exhausted.
    println(f"5.13: ${testStream.mapViaUnfold(_+1).toList}")
    println(f"5.13: ${testStream.takeViaUnfold(3).toList}")
    println(f"5.13: ${testStream.takeWhileViaUnfold(_<3).toList}")
    println(f"5.13: ${testStream.takeWhileViaUnfold(_<0).toList}")
    println(f"5.13: ${testStream.zipWith(testStream)((x:Int, y:Int) => x + 2 * y).toList}")
    println(f"5.13: ${testStream.zipAll(testStream.append(testStream)).toList}")

    // ex 5.14: Hard: Implement startsWith using functions you’ve written.
    // It should check if one Stream is a prefix of another.
    // For instance, Stream(1,2,3) startsWith Stream(1,2) would be true.
    // println(f"5.14: ${testStream.zipAll(Stream(1,2)).toList}")
    // println(f"5.14: ${testStream.zipAll(Stream(2,1)).toList}")
    // println(f"5.14: ${testStream.zipAll(Stream(1,2,3,4,5)).toList}")
    // println(f"5.14: ${testStream.zipAll(Stream(1,2,3,4,5,6)).toList}")
    // val debug = testStream.zipAll(Stream(1,2,3,4,5,6)).filter({
    //   case (None,b) => false
    //   case _ => true
    // })
    // println(f"5.14: ! ${debug.toList}")
    // println(f"5.14: ${testStream.zipAll(Stream(2,1)).toList}")
    // println(f"5.14: ${testStream.zipAll(Stream(1,2,3,4,5)).toList}")
    // println(f"5.14: ${testStream.zipAll(Stream(1,2,3,4,5,6)).toList}")
    println(f"5.14: ${testStream.startsWith(Stream(1,2))}")
    println(f"5.14: ${testStream.startsWith(Stream(2,1))}")
    println(f"5.14: ${testStream.startsWith(Stream(1,2,3,4,5,6))}")
    println(f"5.14: ${testStream.startsWith(Stream(1,2,3,4,5))}")

    // ex 5.15: Implement tails using unfold.
    // For a given Stream, tails returns the Stream of suffixes of the input sequence,
    // starting with the original Stream.
    // For example, given Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).
    println(f"5.15: ${testStream.tails.map(_.toList).toList}")

    // ex 5.16: Hard: Generalize tails to the function scanRight, which is like a foldRight that
    // returns a stream of the intermediate results.
    // For example:
    // scala> Stream(1,2,3).scanRight(0)(_ + _).toList
    // res0: List[Int] = List(6,5,3,0)
    println(f"5.15: ${Stream(1,2,3).scanRight(0)(_+_).toList}")

  }
}
