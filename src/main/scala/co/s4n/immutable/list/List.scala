package co.s4n.immutable.list

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Const[+A](h: A, t: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail: _*))
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Const(h, t) => h + sum(t)
  }

  //Ejercicio 2
  def tail[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Const(_, t) => t
  }

  //Ejercicio 3
  def head[A](lst: List[A]): Any = lst match {
    case Nil => Nil
    case Const(h, _) => h
  }

  //Ejercicio 4
  @tailrec
  def and(lst: List[Boolean]): Boolean = lst match {
    case Nil => false
    case Const(false, Nil) => false
    case Const(false, _) => false
    case Const(true, Nil) => true
    case Const(true, t) => and(t)
  }

  //Ejercicio 5
  def or(lst: List[Boolean]): Boolean = lst match {
    case Nil => false
    case Const(false, Nil) => false
    case Const(false, t) => and(t)
    case Const(true, Nil) => true
    case Const(true, _) => true
  }

  //Ejercicio 6
  def max(lst: List[Int]): Int = {
    @tailrec
    def maxTail(lst: List[Int], maxValue: Int): Int = lst match {
      case Nil => maxValue
      case Const(h, t) if h > maxValue => maxTail(t, h)
      case Const(h, t) if h < maxValue => maxTail(t, maxValue)
      case Const(_, _) => maxValue
    }

    maxTail(lst, Int.MinValue)
  }

  //Ejercicio 7
  def min(lst: List[Long]): Long = {
    @tailrec
    def minTail(lst: List[Long], minValue: Long): Long = lst match {
      case Nil => minValue
      case Const(h, t) if h < minValue => minTail(t, h)
      case Const(h, t) if h > minValue => minTail(t, minValue)
      case Const(_, _) => minValue
    }

    minTail(lst, Long.MaxValue)
  }

  //Ejercicio 8
  def minMax(lst: List[Double]): (Double, Double) = {
    @tailrec
    def minMaxTail(lst: List[Double], result: (Double, Double)): (Double, Double) = lst match {
      case Nil => result
      case Const(h, t) if h < result._1 && h > result._2 => minMaxTail(t, (h, h))
      case Const(h, t) if h < result._1 && h <= result._2 => minMaxTail(t, (h, result._2))
      case Const(h, t) if h > result._1 && h > result._2 => minMaxTail(t, (result._1, h))
      case Const(h, t) if h > result._1 && h > result._2 => minMaxTail(t, (result._1, result._2))
      case Const(_, _) => result
    }

    minMaxTail(lst, (Double.MaxValue, Double.MinValue))
  }

  def length[A](lst: List[A]): Int = lst match {
    case Nil => 0
    case Const(_, t) => 1 + length(t)
  }

  def addEnd[A](lst: List[A], elem: A): List[A] = lst match {
    case Nil => Const(elem, Nil)
    case Const(h, t) => Const(h, addEnd(t, elem))
  }

  def append[A](lst1: List[A], lst2: List[A]): List[A] = (lst1, lst2) match {
    case (Nil, Nil) => Nil
    case (lst1, Nil) => lst1
    case (Nil, lst2) => lst2
    case (Const(h, t), lst2) => Const(h, append(t, lst2))
  }

  @tailrec
  def drop[A](n: Int, lst: List[A]): List[A] = (n, lst) match {
    case (0, lst) => lst
    case (_, Nil) => Nil
    case (n, Const(_, t)) => drop(n - 1, t)
  }

  //Taller 2 Ejercicio 1
  def take[A](n: Int, lst: List[A]): List[A] = (n, lst) match {
    case (0, _) => Nil
    case (_, Nil) => Nil
    case (n, Const(h, t)) => Const(h, take(n - 1, t))
  }

  //Taller 2 Ejercicio 2
  def init[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Const(_, Nil) => Nil
    case Const(h, t) => Const(h, init(t))
  }

  //Taller 2 Ejercicio 3
  def split[A](lst: List[A], n: Int): (List[A], List[A]) = {
    @tailrec
    def splitTail[A](lst: List[A], n: Int, accumulator: List[A]): (List[A], List[A]) = (lst, n) match {
      case (_, 0) => (accumulator, lst)
      case (Nil, _) => (accumulator, lst)
      case (Const(h, t), _) => splitTail(t, n - 1, addEnd(accumulator, h))
    }

    splitTail(lst, n, Nil)
  }

  //Taller 2 Ejercicio 4
  def zip[A, B](lst1: List[A], lst2: List[B]): List[(A, B)] = (lst1, lst2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Const(h, t), Const(h2, t2)) => Const((h, h2), zip(t, t2))
  }

  //Taller 2 Ejercicio 5
  def unzip[A, B](lst: List[(A, B)]): (List[A], List[B]) = {
    @tailrec
    def unzipTail[A, B](lst: List[(A, B)], newLst1: List[A], newLst2: List[B]): (List[A], List[B]) = lst match {
      case Nil => (newLst1, newLst2)
      case Const((h1, h2), t) => unzipTail(t, addEnd(newLst1, h1), addEnd(newLst2, h2))
    }

    unzipTail(lst, Nil, Nil)
  }

  //Taller 2 Ejercicio 6
  def reverse[A](lst: List[A]): List[A] = {
    @tailrec
    def reverseTail[A](lst: List[A], reversedLst: List[A]): List[A] = lst match {
      case Nil => reversedLst
      case Const(h, t) => reverseTail(t, Const(h, reversedLst))
    }

    reverseTail(lst, Nil)
  }

  //Taller 2 Ejercicio 7
  def intersperse[A](lst: List[A], elem: A): List[A] = lst match {
    case Nil => Nil
    case Const(h, Nil) => Const(h, Nil)
    case Const(h, t) => Const(h, Const(elem, intersperse(t, elem)))
  }

  //Taller 2 Ejercicio 8
  def concat[A](lst: List[List[A]]): List[A] = {
    @tailrec
    def concatTail[A](lst: List[List[A]], accumulator: List[A]): List[A] = lst match {
      case Nil => accumulator
      case Const(Nil, outer) => concatTail(outer, accumulator)
      case Const(Const(h, t), outer) => concatTail(Const(t, outer), Const(h, accumulator))
    }

    reverse(concatTail(lst, Nil))
  }

}

