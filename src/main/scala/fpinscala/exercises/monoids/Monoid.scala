package fpinscala.exercises.monoids

import fpinscala.exercises.parallelism.Nonblocking.*
import fpinscala.exercises.parallelism.Nonblocking.Par.{asyncF, delay, eval}

import java.util.concurrent.{ExecutorService, Executors}

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

object Monoid:

  val stringMonoid: Monoid[String] = new:
    def combine(a1: String, a2: String) = a1 + a2
    val empty = ""

  def listMonoid[A]: Monoid[List[A]] = new:
    def combine(a1: List[A], a2: List[A]) = a1 ++ a2
    val empty = Nil

  lazy val intAddition: Monoid[Int] = new:
    def combine(a1: Int, a2: Int): Int = a1 + a2
    val empty = 0

  lazy val intMultiplication: Monoid[Int] = new:
    def combine(a1: Int, a2: Int): Int = a1 * a2
    val empty = 1

  lazy val booleanOr: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean) = a1 || a2
    val empty = false

  lazy val booleanAnd: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean) = a1 && a2
    val empty = true

  def optionMonoid[A]: Monoid[Option[A]] = new:
    def combine(a1: Option[A], a2: Option[A]) = a1.orElse(a2)
    val empty = None

  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(x: A, y: A): A = m.combine(y, x)
    val empty = m.empty

  def endoMonoid[A]: Monoid[A => A] = new:
    def combine(a1: A => A, a2: A => A) = a1.andThen(a2)
    val empty = (a:A) => a
  
  import fpinscala.exercises.testing.{Prop, Gen}
  // import Gen.`**`

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def combineAll[A](as: List[A], m: Monoid[A]): A =
    ???

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.empty)((acc, a) => m.combine(acc, f(a)))

  def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(acc)

  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    val t = (a: A) => (b: B) => f(b, a)
    foldMap(as, endoMonoid[B])(t)(acc)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if as.isEmpty then m.empty
    else if as.size == 1 then f(as.head)
    else
      val (l, r) = as.splitAt(as.size / 2)
      m.combine(foldMapV(l, m)(f), foldMapV(r, m)(f))

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new:
    def combine(p1: Par[A], p2: Par[A]) = p1.map2(p2)(m.combine)
    val empty = Par.unit(m.empty)
    

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    val t = par(m).empty
    foldMapV(v, par(m))(asyncF(f))

  def ordered(ints: IndexedSeq[Int]): Boolean =
    case class OrderStatus(value: Int, ordered: Boolean)
    lazy val intDiff: Monoid[OrderStatus] = new:
      def combine(a1: OrderStatus, a2: OrderStatus): OrderStatus =
        OrderStatus(a2.value, a1.ordered && a2.ordered && a1.value <= a2.value)
      val empty = OrderStatus(Int.MinValue, true)
    foldMapV(ints, intDiff)(i => OrderStatus(i, true)).ordered

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  lazy val wcMonoid: Monoid[WC] = ???

  def count(s: String): Int = ???

  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] with
    def combine(x: (A, B), y: (A, B)) = ???
    val empty = ???

  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(f: A => B, g: A => B) = ???
    val empty: A => B = a => ???

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a: Map[K, V], b: Map[K, V]) = ???
    val empty = ???

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???

end Monoid
