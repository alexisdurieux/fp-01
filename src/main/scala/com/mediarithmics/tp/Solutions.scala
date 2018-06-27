package com.mediarithmics.tp

object Solutions {
  def double(f: Int => Int): Int => Int =
    (x: Int) => 2 * f(x)

  def compose[A, B, C](f: A => B, g: B => C): A => C =
    (a: A) => g(f(a))

  def map[A, B](f: A => B): Option[A] => Option[B] = {
    case Some(a) => Some(f(a))
    case None => None
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)


  sealed trait BinaryTree[+A]

  case class Branch[A](l: BinaryTree[A], v: A, r: BinaryTree[A]) extends BinaryTree[A]

  case object Leaf extends BinaryTree[Nothing]

  sealed trait Order

  case object LT extends Order

  case object EQ extends Order

  case object GT extends Order

  def insert[A](tree: BinaryTree[A], a: A, cmp: (A, A) => Order): BinaryTree[A] =
    tree match {
      case Branch(l, v, r) =>
        if (cmp(v, a) == EQ)
          tree
        else if (cmp(v, a) == LT)
          Branch(insert(l, a, cmp), v, r)
        else
          Branch(l, v, insert(r, a, cmp))
      case Leaf =>
        Branch(Leaf, a, Leaf)
    }

  def fromList[A](l: List[A], cmp: (A, A) => Order): BinaryTree[A] =
    l.foldLeft[BinaryTree[A]](Leaf)((t, v) => insert(t, v, cmp))

  def contains[A](tree: BinaryTree[A], v: A, cmp: (A, A) => Order): Boolean =
    tree match {
      case Branch(l, v2, r) =>
        if (cmp(v, v2) == EQ)
          true
        else if (cmp(v, v2) == LT)
          contains(l, v, cmp)
        else
          contains(r, v, cmp)
      case Leaf =>
        false
    }

  def contains[A](tree: BinaryTree[A], v: A): Boolean =
    tree match {
      case Branch(l, v2, r) =>
        if (v == v2)
          true
        else
          contains(l, v) || contains(r, v)
      case Leaf =>
        false
    }

  def foldTree[A, B](tree: BinaryTree[A], z: B)(f: (B, A, B) => B): B =
    tree match {
      case Branch(l, v, r) =>
        f(foldTree(l, z)(f), v, foldTree(r, z)(f))
      case Leaf =>
        z
    }

  type TreeMap[A, B] = BinaryTree[(A, B)]

  def get[A, B](map: TreeMap[A, B], k: A, cmp: (A, A) => Order): Option[B] = {
    foldTree[(A, B), Option[B]](map, None) {
      case (l, kv, r) =>
        if (cmp(k, kv._1) == EQ)
          Some(kv._2)
        else if (cmp(k, kv._1) == LT)
          l
        else
          r
    }
  }

  def values[A, B](map: TreeMap[A, B]): List[B] = {
    foldTree[(A, B), List[B]](map, Nil) {
      case (l, kv, r) =>
        l ++: List(kv._2) ++: r
    }
  }
}
