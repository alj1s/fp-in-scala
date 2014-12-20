/**
 * Created by andrewjones on 06/10/2014.
 */
package fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.1
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case _ => 101
  }

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  // Exercise 3.3
  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(a, xs)
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    def loop(l: List[A], n: Int): List[A] =
      if (n == 0) l
      else loop(tail(l), n - 1)

    loop(l, n)
  }

  // Exercise 3.5
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)((x, y) => x * y)

  // Exercise 3.9
  def length[A](ns: List[A]): Int =
    foldRight(ns, 0)((_, y) => 1 + y)

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // Exercise 3.11
  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)((x, y) => x + y)

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)((x, y) => x * y)

  def length2[A](ns: List[A]): Int =
    foldLeft(ns, 0)((x, y) => x + 1)

  // Exercise 3.12
  def reverse[A](ns: List[A]): List[A] =
    foldLeft(ns, List[A]())((x, y) => Cons(y, x))

  // Exercise 3.13
  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(as), z)((a, b) => f(b, a))

  // Exercise 3.14
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a2, a1)((x, y) => Cons(x, y))
  }

  // Exercise 3.15
  def concatenate[A](a1: List[List[A]]): List[A] = {
    foldRight(a1, Nil: List[A])((x, y) => append(x, y))
  }

  // Exercise 3.16
  def addOne(a: List[Int]): List[Int] = {
    foldRight(a, Nil: List[Int])((x, y) => Cons(x + 1, y))
  }

  // Exercise 3.17
  def toString(a: List[Double]): String = {
    foldRight(a, "")((x, y) => y + a.toString)
  }

  def toString2(a: List[Double]): List[String] = {
    foldRight(a, Nil: List[String])((x, y) => Cons(x.toString, y))
  }

  // Exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((x, y) => Cons(f(x), y))
  }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((x, y) => if (f(x)) Cons(x, y) else y)
  }

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil: List[B])((x, y) => append(f(x), y))
  }

  def flatMap2[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concatenate(map(as)(f))
  }

  // Exercise 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  // Exercise 3.22
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Cons(x, as), Cons(y, bs)) => Cons(x + y, addPairwise(as, bs))
    case _ => Nil
  }

  // Exercise 3.23
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Cons(x, as), Cons(y, bs)) => Cons(f(x, y), zipWith(as, bs)(f))
    case _ => Nil
  }

  // Exercise 3.24
  def hasSubsequence[A](a: List[A], b: List[A]): Boolean = (a, b) match {
    case (Cons(x, as), Cons(y, bs)) => if (x == y) hasSubsequence(as, bs) else hasSubsequence(as, b)
    case (_, Nil) => true
    case _ => false
  }
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  // Exercise 3.26
  def max(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(left, right) => max(left) max max(right)
  }

  // Exercise 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  // Exercise 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))

  }

  // Exercise 3.29
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def size2[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)(1 + _ + _)
  }

  def max2(t: Tree[Int]): Int = {
    fold(t)(x => x)(_ max _)
  }

  def depth2[A](t: Tree[A]): Int = {
    fold(t)(_ => 0)((x, y) => 1 + (x max y))
  }

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_, _))
  }
}
