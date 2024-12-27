package fpinscala.exercises.state

import scala.annotation.tailrec


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val i = rng.nextInt
    i match
      case _ if i._1 < 0 => (-(i._1 + 1), i._2)
      case _ => i

  def double(rng: RNG): (Double, RNG) =
    val i = nonNegativeInt(rng)
    (i._1.toDouble / (Int.MaxValue.toDouble + 1), i._2)

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    val next1 = rng.nextInt
    val i = next1._1
    val d = double(next1._2)
    ((i, d._1), d._2)

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    val r = intDouble(rng)
    ((r._1._2, r._1._1), r._2)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val d1 = double(rng)
    val d2 = double(d1._2)
    val d3 = double(d2._2)
    ((d1._1, d2._1, d3._1), d3._2)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    @tailrec
    def intsIter(acc: List[Int], rng: RNG, range: List[Int]): (List[Int], RNG) =
      val i = rng.nextInt
      range match
        case List() => (acc, rng)
        case head :: tail => intsIter(i._1 :: acc, i._2, range.tail)
    intsIter(List.empty, rng, Range.inclusive(1, count).toList)

  def _double: Rand[Double] = map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)

  // voir réponse : accInit est en fait unit
  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    val accInit : Rand[List[A]] = rng => (List.empty[A], rng)
    rs.foldLeft(accInit) { (acc, rni) =>
      map2(rni, acc) { (a, b) => a :: b }
    }

  // voir réponse : rand(i) est int défini plus haut, et i est inutilisé.
  def intsViaSequence(count: Int): Rand[List[Int]] =
    def rand(i: Int): Rand[Int] = rng => rng.nextInt
    sequence(Range.inclusive(1, count).toList.map(i => rand(i)))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, rng1) = r(rng)
      val rand  = f(a)
      rand(rng1)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)((a: A) => (rng: RNG) => (f(a), rng))

  // voir la réponse du livre, plus simple avec map.
  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    val t = flatMap(ra)((a: A) => (rng: RNG) =>
      val rng2 = rb(rng)
      val t2 = ((a, rng2._1), rng2._2)
      t2
    )
    flatMap(t)((a: A, b: B) => (rng: RNG) => (f(a, b), t(rng)._2))

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      state =>
        val (a, nextState) = underlying(state)
        (f(a), nextState)

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      state =>
        val (a, nextState1) = underlying(state)
        val (b, nextState2) = sb(nextState1)
        (f(a, b), nextState2)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      state =>
        val (a, nextState) = underlying(state)
        f(a)(nextState)
    
  def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] =
      val initialState: State[S, List[A]] = (state: S) => (List.empty[A], state) 
      rs.foldRight(initialState){(s, acc) =>
        s.map2(acc)((a: A, l: List[A]) => a :: l)
      }

  def unit[S, A](a: A): State[S, A] =
    state => (a, state)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
