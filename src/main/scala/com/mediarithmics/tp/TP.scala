package com.mediarithmics.tp

object TP {

  def double(f: Int => Int): Int => Int = ???

  def compose[A, B, C](f: A => B, g: B => C): A => C = ???

  def map[A, B](f: A => B): Option[A] => Option[B] = ???

  def curry[A, B, C](f: (A, B) => C): A => B => C = ???

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = ???
  
  sealed trait BinaryTree[+A]

  sealed trait Order

  case object LT extends Order

  case object EQ extends Order

  case object GT extends Order

  def insert[A](tree: BinaryTree[A], cmp: (A, A) => Order, a: A): BinaryTree[A] = ???

  def fromList[A](l: List[A], cmp: (A, A) => Order): BinaryTree[A] = ???

  def contains[A](tree: BinaryTree[A], v: A, cmp: (A, A) => Order): Boolean = ???

  def contains[A](tree: BinaryTree[A], v: A): Boolean = ???

  def foldTree[A, B](tree: BinaryTree[A], z: B)(f: (B, A, B) => B): B = ???

  type TreeMap[A, B] = BinaryTree[(A, B)]

  def get[A, B](map: TreeMap[A, B], k: A, cmp: (A, A) => Order): Option[B] = ???

  def values[A, B](map: TreeMap[A, B]): List[B] = ???
}
