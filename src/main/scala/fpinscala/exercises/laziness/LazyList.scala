package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.{cons, empty, unfold}

import scala.annotation.tailrec

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] =
    @tailrec
    def toListIter(acc: List[A], lzy: LazyList[A]): List[A] =
      lzy match
        case Empty => acc
        case Cons(h, t) => toListIter(h() :: acc, t())
    toListIter(List.empty[A], this).reverse

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  // Voir la réponse du livre : est-ce que le traitement du cas n == 1 est différent de ce qui est fait ici ?
  // Surtout, voir l'explication sur la question de thread safe ou non.
  // L'appel imbriqué de take a lieu sur l'instance de LazyList correspondant à la tail.
  // Pourquoi n'est-ce pas un appel récursif de take depuis take ?
  // Car le second argument de cons est by name.
  def take(n: Int): LazyList[A] =
    //println(s"take $n : ${this.toList}")
    //println(s"take $n")
    this match
      case Empty => Empty
      case Cons(h, t) if n <= 0 => Empty
      case Cons(h, t) => LazyList.cons(h(), t().take(n - 1))

  // Nécessaire d'ajouter final ici pour permettre @tailrec, sinon peut être surchargée.
  // Le comportement thread safe de take me semble applicable ici aussi, donc est-ce vraiment une récursivité ?
  // C'est à dire, puisque tail est lazy, il n'y a pas de récursion.
  // Cependant, on ne peut pas ajouter l'annotation @tailrec à take.
  @tailrec
  final def drop(n: Int): LazyList[A] =
    //println(s"drop $n : ${this.toList}")
    this match
      case Empty => Empty
      case Cons(h, t) if n <= 0 => this
      case Cons(h, t) => t().drop(n - 1)

  def takeWhile(p: A => Boolean): LazyList[A] =
    /*this match
      case Empty => Empty
      case Cons(h, t) if ! p(h()) => Empty
      case Cons(h, t) => LazyList.cons(h(), t().takeWhile(p))*/
    // En utilisant foldRight :
    foldRight(LazyList.empty)((a, b) => if !p(a) then LazyList.empty else cons(a, b))

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = /*this match
    case Empty => None
    case Cons(h, _) => Some(h())
     */
    // En utilisant foldRight :
    foldRight(None: Option[A])((a, b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): LazyList[B] =
    foldRight(empty)((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if f(a) then cons(a, b) else b)

  def append[B>:A](other: => LazyList[B]): LazyList[B] =
    foldRight(other)((a, b) => cons(a, b))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty)((a, b) => f(a).append(b))

  def mapViaUnfold[B](f: A => B): LazyList[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  }

  def takeViaUnfold(n: Int): LazyList[A] = unfold((this, n)) { state => state._1 match
    case Empty => None
    case Cons(h, t) if state._2 <= 0 => None
    case Cons(h, t) => Some((h(), (t(), state._2 - 1)))
  }

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWith[B, C](b: LazyList[B])(f: (A, B) => C): LazyList[C] = unfold((this, b)) {
    case (Empty, _) => None
    case (_, Empty) => None
    case (Cons(h, t), Cons(bh, bt)) => Some(f(h(), bh()), (t(), bt()))
  }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] = unfold((this, that)) {
    case (Empty, Empty) => None
    case (Empty, Cons(bh, bt)) => Some((None, Some(bh())), (Empty, bt()))
    case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
    case (Cons(h, t), Cons(bh, bt)) => Some((Some(h()), Some(bh())), (t(), bt()))
  }

  def startsWith[B](s: LazyList[B]): Boolean = (this, s) match
    case (Empty, Empty) => true
    case (Empty, _) => false
    case (_, Empty) => true
    case _ => zipWith(s)((a, b) => (a, b)).forAll(t => t._1 == t._2)

  def tails: LazyList[LazyList[A]] =
    val lazyListWithoutElements = cons(LazyList.empty, LazyList.empty)
    this match
    case Empty => lazyListWithoutElements
    case _ => unfold(this) {
      case Empty => None
      case current => Some((current, current.drop(1)))
    }.append(lazyListWithoutElements)

  def hasSubsequence[B](l: LazyList[B]): Boolean =
    tails.exists(_.startsWith(l))


object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    //println(s"cons : $hd , $tl")
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = LazyList.cons(a, continually(a))

  def from(n: Int): LazyList[Int] = LazyList.cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] =
    def fibsIter(prev: Int, curr: Int): LazyList[Int] = LazyList.cons(prev + curr, fibsIter(curr, prev + curr))
    LazyList.cons(0, LazyList.cons(1, fibsIter(0, 1)))

  // Voir solution du livre. Je préfère peut-être celle-ci.
  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state).map((a, s) => LazyList.cons(a, unfold(s)(f))).getOrElse(LazyList.empty)

  // Voir solution du livre : le pattern matching évite la définition de la case class, et permet d'éviter
  // de devoir fixer les deux premières valeurs avant de créer un fibState avec ces deux valeurs intiales.
  lazy val fibsViaUnfold: LazyList[Int] =
    case class fibState(prev: Int, curr: Int)
    LazyList.cons(0, LazyList.cons(1,
      unfold(fibState(0, 1))(state => Some((state.prev + state.curr, fibState(state.curr, state.prev + state.curr))))))

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(n => Some((n, n + 1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(a)(a => Some((a, a)))

  lazy val onesViaUnfold: LazyList[Int] = unfold(1)(one => Some((one, one)))

