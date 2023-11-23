package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.cons

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
  // Ce n'est pas un appel récursif de take depuis take.
  def take(n: Int): LazyList[A] =
    //println(s"take $n : ${this.toList}")
    this match
      case Empty => Empty
      case Cons(h, t) if n <= 0 => Empty
      case Cons(h, t) => LazyList.cons(h(), t().take(n - 1))

  // Nécessaire d'ajouter final ici pour permettre @tailrec, sinon peut être surchargée.
  // Le comportement thread safe de take me semble applicable ici aussi, donc est-ce vraiment une récursivité ?
  // Cependant, on ne peut pas ajouter l'annotation @tailrec à take.
  @tailrec
  final def drop(n: Int): LazyList[A] =
    //println(s"drop $n : ${this.toList}")
    this match
      case Empty => Empty
      case Cons(h, t) if n <= 0 => this
      case Cons(h, t) => t().drop(n - 1)

  def takeWhile(p: A => Boolean): LazyList[A] = ???

  def forAll(p: A => Boolean): Boolean = ???

  def headOption: Option[A] = this match
    case Empty => None
    case Cons(h, _) => Some(h())

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: LazyList[B]): Boolean = ???


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

  def continually[A](a: A): LazyList[A] = ???

  def from(n: Int): LazyList[Int] = ???

  lazy val fibs: LazyList[Int] = ???

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = ???

  lazy val fibsViaUnfold: LazyList[Int] = ???

  def fromViaUnfold(n: Int): LazyList[Int] = ???

  def continuallyViaUnfold[A](a: A): LazyList[A] = ???

  lazy val onesViaUnfold: LazyList[Int] = ???
