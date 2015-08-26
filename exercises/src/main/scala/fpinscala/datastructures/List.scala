package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(x, xs) => Cons(h, xs)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case l: List[A] if n == 0 => l
    case Cons(x, xs) if n > 0 => drop(xs, n - 1)
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((item, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldLeft(l, Nil: List[B])((acc, item) => Cons(f(item), acc))

  def sumFL(l: List[Int]) = foldLeft(l, 0)((acc, item) => acc + item)

  def productFL(l: List[Double]) =
    foldLeft(l, 1.0)((acc, item) => acc * item)

  def lengthFL[A](l: List[A]) = foldLeft(l, 0)((acc, item) => acc + 1)

  def reverse[A](l: List[A]) = foldLeft(l, List[A]())((acc, item) => Cons(item, acc))

  def foldLeftWithFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((acc, item) => b => item(f(b, acc)))(z)

  def foldRightWithFoldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldLeft(l, (b: B) => b)((acc, item) => b => acc(f(b, item)))(z)

  def appendFR[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((acc, item) => Cons(acc, item))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])((acc, item) => appendFR(acc, item))

  def addOne(l: List[Int]): List[Int] = map(l)(_ + 1)

  def doubleToString(l: List[Double]): List[String] = map(l)(_.toString)

  def filter[A](l: List[A])(p: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((item, acc) =>
      if (p(item)) Cons(item, acc)
      else acc
    )

  def removeOdd(l: List[Int]): List[Int] = filter(l)(_ % 2 == 0)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

  def filterFromFlatMap[A](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l)(item => if (p(item)) List(item) else Nil)

  def zipWith[A, B, C](la: List[A], lb: List[B], f: (A, B) => C): List[C] = (la, lb) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb, f))
  }

  def sumZip(la: List[Int], lb: List[Int]): List[Int] = zipWith[Int, Int, Int](la, lb, (a, b) => a + b)

  @annotation.tailrec
  def startsWith[A](a: List[A], sub: List[A]): Boolean = (a, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(h, t), Cons(sh, st)) if h == sh => startsWith(t, st)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](a: List[A], sub: List[A]): Boolean = a match {
    case Nil => sub == Nil
    case l if startsWith(l, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }
}