package chapter3

/**
 * Created by nicole on 7/12/16.
 */
sealed trait Tree[+A]
case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]
case class Leaf[A](v: A) extends Tree[A]

object Tree {

  // 3.25
  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) =>
        1 + size(l) + size(r)
    }

  // 3.26
  def maximum[A](tree: Tree[Int]): Int =
    tree match {
      case Leaf(v) => v
      case Branch(l, r) =>
        maximum(l) max maximum(r)
    }

  //3.27
  def depth[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) =>
        1 + (depth(l) max depth(r))
    }

  // 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) =>
        Branch(map(l)(f), map(r)(f))
    }

  // 3.29
  def fold[A,B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B =
    tree match {
      case Leaf(a) => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def sizeWithFold[A](tree: Tree[A]): Int =
    fold(tree)(a => 1)(_ + _)

  def maximumWithFold(tree: Tree[Int]) =
    fold(tree)(a => a)(_ max _)

  def depthWithFold[A](tree: Tree[A]) =
    fold(tree)(a => 1) { (b1, b2) =>
      (1 + b1) max (1 + b2)
    }

  def mapWithFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}