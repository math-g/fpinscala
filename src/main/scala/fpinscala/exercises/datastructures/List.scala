package fpinscala.exercises.datastructures

import scala.annotation.tailrec

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match
      case Nil => sys.error("Cannot call tail on an empty List.")
      case Cons(head, tail) => tail

  def setHead[A](l: List[A], h: A): List[A] =
    Cons(h, tail(l))

  def drop[A](l: List[A], n: Int): List[A] =
    @tailrec
    def dropIter(dropped: List[A], i: Int): List[A] =
      dropped match
        case Nil => Nil
        case _ if i == n => dropped
        case _ => dropIter(tail(dropped), i + 1)
    if n <= 0 then l else dropIter(l, 0)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    @tailrec
    def dropIter(dropped: List[A]): List[A] =
      dropped match
        case Nil => Nil
        case Cons(head, tail) if !f(head) => dropped
        case _ => dropIter(tail(dropped))
    dropIter(l)

  // Voir la solution, équivalent à un foldRight, plus efficace mais non tail recursive.
  def init[A](l: List[A]): List[A] =
    @tailrec
    def reverse(acc: List[A], curr: List[A]): List[A] =
      curr match
        case Nil => acc
        case Cons(h, t) => reverse(Cons(h, acc), tail(curr))
    @tailrec
    def initIter(acc: List[A], curr: List[A]): List[A] =
      curr match
        case Nil => sys.error("Cannot call init on an empty List.")
        case Cons(head, Nil) => acc
        case Cons(h, t) => initIter(Cons(h, acc), tail(curr))
    reverse(Nil, initIter(Nil, l))

  def length[A](l: List[A]): Int =
    foldRight(l, 0: Int, (x, y) => 1 + y)

  @tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B =
    l match
      case Nil => acc
      case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (acc, elem) => 1 + acc)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A], (acc, h) => Cons(h, acc))

  def foldRightViaFoldLeft[A,B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, (b, a) => f(a, b))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, (curr: A, acc: List[A]) => Cons(curr, acc))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A], (lst: List[A], acc: List[A]) => append(lst, acc))

  def incrementEach(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int], (elem, acc) => Cons(elem + 1, acc))
    //foldLeft(l, Nil: List[Int], (acc, elem) => Cons(elem + 1, acc)) => fail : inverse la List

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String], (elem, acc) => Cons(elem.toString, acc))

  def map[A,B](l: List[A], f: A => B): List[B] =
    foldRight(l, Nil: List[B], (elem, acc) => Cons(f(elem), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    // implémentation sans foldRight
    /*as match
      case Nil => as
      case Cons(h, t) if f(h) => Cons(h, filter(t, f))
      case Cons(h, t) => filter(t, f)
     */
    foldRight(as, Nil: List[A], (elem, acc) => if f(elem) then Cons(elem, acc) else acc)


  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B], (elem, acc) => append(f(elem), acc))
    // alternative :
    // concat(map(as, f))


  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then Cons(a, Nil) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    @tailrec
    def addPairWiseIter(acc: List[Int], aIter: List[Int], bIter: List[Int]): List[Int] =
      aIter match
        case Nil => acc
        case Cons(ha, ta) =>
          bIter match
            case Nil => acc
            case Cons(hb, tb) => addPairWiseIter(Cons(ha + hb, acc), ta, tb)
    reverse(addPairWiseIter(Nil: List[Int], a, b))

    /*
    Solution du livre : possible de matcher sur deux valeurs. Pattern façon foldRight, non tail recursif.
    (a, b) match
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
     */

  // def zipWith - TODO determine signature
  def zipWith[A](a: List[A], b: List[A], f: (A, A) => A): List[A] =
    @tailrec
    def addPairWiseIter(acc: List[A], aIter: List[A], bIter: List[A]): List[A] =
      (aIter, bIter) match
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(ha, ta), Cons(hb, tb)) => addPairWiseIter(Cons(f(ha, hb), acc), ta, tb)
    reverse(addPairWiseIter(Nil: List[A], a, b))

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
