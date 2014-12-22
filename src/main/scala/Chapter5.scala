/**
 * Created by andrewjones on 22/12/14.
 */

//package fpinscala.datastructures.Chapter5

import Stream._

sealed trait Stream[+A] {

  // Exercise 5.1 - may cause stackOverflow
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()

  }

  def toList_tailRec: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }

  // Exercise 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 1) => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty

  }

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean) = {
    foldRight(false)((h, t) => p(h) || t)
  }

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((h, t) => p(h) && t)
  }

  // Exercise 5.5
  def takeWhile_1(p: A => Boolean): Stream[A] = {
    foldRight(empty: Stream[A])((h, t) => if (p(h)) cons(h, t) else empty)
  }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // Exercise 5.6
  def headOption_1: Option[A] = {
    foldRight(None: Option[A])((h, t) => Some(h))
  }

  // Exercise 5.7
  def append[B >: A](a1: => Stream[B]): Stream[B] = {
    foldRight(a1)((h, t) => cons(h, t))
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((x, y) => cons(f(x), y))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty: Stream[A])((x, y) => if (f(x)) cons(x, y) else y)
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty: Stream[B])((x, y) => f(x) append (y))
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](b: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // Exercise 5.8
  def constant[A](a: A): Stream[A] = {
    lazy val constants = Stream.cons(a, constant(a))
    constants
  }

}