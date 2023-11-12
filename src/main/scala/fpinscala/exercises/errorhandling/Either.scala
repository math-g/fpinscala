package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal

enum Either[+E,+A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] =
    this match
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match
      case Left(e) => Left(e)
      case Right(a) => f(a)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match
      case Right(a) => this
      case Left(e) => b

  // Un seul Either en argument contrairement à Option.map2 car ici c'est une méthode de l'instance.
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap(a => b.map(b => f(a, b)))
    /*
    Alternative du livre :
    for
      a <- this
      b <- that
    yield f(a, b)
     */

object Either:
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight(Right(List.empty: List[B]): Either[E, List[B]])((a, ret) => ret.flatMap(acc => f(a).map(b => b :: acc)))
    // Assai alternative : passe mais inutilement plus complexe par rapport à ci-après.
    //es.foldRight[Either[E, List[B]]](Right(List.empty))((a, ret) => ret.map2(f(a))((lst, b) => b :: lst))
    // Solution livre : noter comme il est possible et préférable de typer foldRight plutôt que l'accumulateur
    // as.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))


  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(ea => ea)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if xs.isEmpty then
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] = 
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B], f: (A, B) => C): Either[List[E], C] = ???

  def traverseAll[E, A, B](as: List[A], f: A => Either[List[E], B]): Either[List[E], List[B]] = ???

  def sequenceAll[E, A](as: List[Either[List[E], A]]): Either[List[E], List[A]] = ???
