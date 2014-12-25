/**
 * Created by andrewjones on 24/12/14.
 */

package fpinscala.Chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  // Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (n, nextRng) if (n < 0) => (1 - n, nextRng)
      case _ => _
    }
  }

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRng) = nonNegativeInt(rng)
    (n.toDouble / (Int.MaxValue + 1), nextRng)
  }

  // Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, nextRng) = rng.nextInt
    val (d, nextNextRng) = double(nextRng)
    ((i, d), nextNextRng)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r) = double(rng)
    val (d2, r2) = double(r)
    val (d3, r3) = double(r2)
    ((d, d2, d3), r3)
  }

  // Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (List(), rng)
    else {
      val (i, r) = rng.nextInt
      val (l, r2) = ints(count - 1)(r)
      (i :: l, r2)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = {
    map(nonNegativeInt)(i => i - i % 2)
  }

  // Exercise 6.5
  def double2: Rand[Double] = {
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue + 1))
  }

  // Exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, r2) = ra(rng)
      val (b, r3) = rb(r2)
      (f(a, b), r3)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      fs match {
        case (f :: l) => {
          val (a, r) = f(rng)
          val (a2, r2) = sequence(l)(r)
          ((a :: a2), r2)
        }
        case Nil => (List(), rng)
      }
    }
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  }

  def ints2(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))
}

