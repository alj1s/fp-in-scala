/**
 * Created by andrewjones on 22/12/14.
 */

package fpinscala.Chapter5

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

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
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

  // Exercise 5.13
  def map_2[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  }

  def take_2(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h, t), n) if n == 1 => Some(h(), (empty, n - 1))
      case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
      case _ => None
    }
  }

  def takeWhile_2(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }
  }

  def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, b)) {
      case (Cons(h, t), Cons(b, r)) => Some((f(h(), b()), (t(), r())))
      case _ => None
    }
  }

  def zipAll[B, C](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(((Some(h()), Option.empty[B]), (t(), empty[B])))
      case (Empty, Cons(h, t)) => Some(((Option.empty[A], Some(h())), (empty[A], t())))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    }

  // Exercise 5.14
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h, h2) => h == h2
    }

  // Exercise 5.15
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s.drop(1)))
    } append (Stream(empty))

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails.exists(_.startsWith(s))

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

  // Exercise 5.9
  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }

  // Exercise 5.10
  def fibs: Stream[Int] = {

    def go(prev: Int, current: Int): Stream[Int] = {
      cons(prev, go(current, prev + current))
    }

    go(0, 1)
  }

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _ => empty
    }
  }

  // Exercise 5.12
  val ones_1: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  def constant_1[A](a: A): Stream[A] = {
    unfold(a)(_ => Some((a, a)))
  }

  def from_1(n: Int): Stream[Int] = {
    unfold(n)(_ => Some((n, n + 1)))
  }

  def fibs_1: Stream[Int] = {
    unfold((0, 1)) { case (current, prev) => Some((current, (prev + current, current)))}
  }

}