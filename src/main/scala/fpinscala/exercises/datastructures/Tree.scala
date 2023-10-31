package fpinscala.exercises.datastructures

import fpinscala.exercises.datastructures.Tree.Branch

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_) => 0
    case Branch(l, r) => (1 + l.depth).max(1 + r.depth)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(elem) => Leaf(f(elem))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B,B) => B): B = ???
  
  def sizeViaFold: Int = ???
  
  def depthViaFold: Int = ???
  
  def mapViaFold[B](f: A => B): Tree[B] = ???

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Int = t match
    case Leaf(i) => i
    case Branch(l, r) =>
      val lpos = l.firstPositive
      if lpos > 0 then lpos else r.firstPositive

  extension (t: Tree[Int]) def maximum: Int =
    t match
      case Leaf(i) => i
      case Branch(l, r) =>
        val lval = l.maximum
        val rval = r.maximum
        if lval > rval then lval else rval

  extension (t: Tree[Int]) def maximumViaFold: Int = ???
