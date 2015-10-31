package fpinscala.datastructure

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int =
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

  def product(ds: List[Double]): Double =
    ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _ *))

  // EXERCISE 3.2
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

  // EXERCISE 3.3
  def setHead[A](l: List[A], a: A): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, xs) => Cons(a, xs)
    }

  // EXERCISE 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n-1)
    }

  // EXERCISE 3.5
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) dropWhile(xs)(f) else l
    }

  // EXERCISE 3.6
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y)=> x + y)
  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  // EXERCISE 3.7

  // EXERCISE 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((x, n) => n + 1)

  // EXERCISE 3.10
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  // EXERCISE 3.11
  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
  def length2[A](as: List[A]): Int = foldLeft(as, 0)((n, x) => n + 1)

  // EXERCISE 3.12
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def reverse[A](as: List[A]): List[A] =
    as match {
      case Nil => Nil
      case Cons(x, Nil) => List(x)
      case Cons(h, t) => append(reverse(t), List(h))
    }

  def reverse2[A](as: List[A]): List[A] =
    foldRight(as, Nil:List[A])((x, l) => append(l, List(x)))

  // EXERCISE 3.13
  // def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B =
  //   foldRight(as, (y: B) => y)((f2, xs) => )
    // as match {
    //   case Nil => z
    //   case Cons(x, xs) => foldRight(as, f(z, x))((acc, ys) => f(ys, acc))
    // }

  // EXERCISE 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x, as) => Cons(x, as))
  // def append3[A](a1: List[A], a2: List[A]): List[A] =
  //   foldLeft(a1, a2)((as, x) => Cons(x, as))
}
