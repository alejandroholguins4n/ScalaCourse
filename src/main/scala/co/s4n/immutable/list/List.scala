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
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Const(_, t) => t
  }

  //Ejercicio 3
  def head[A](list: List[A]): Any = list match {
    case Nil => Nil
    case Const(h, _) => h
  }

  //Ejercicio 4
  @tailrec
  def and(list: List[Boolean]): Boolean = list match {
    case Nil => false
    case Const(false, Nil) => false
    case Const(false, _) => false
    case Const(true, Nil) => true
    case Const(true, t) => and(t)
  }

  //Ejercicio 5
  def or(list: List[Boolean]): Boolean = list match {
    case Nil => false
    case Const(false, Nil) => false
    case Const(false, t) => and(t)
    case Const(true, Nil) => true
    case Const(true, _) => true
  }

  //Ejercicio 6
  def max(list: List[Int]): Int = {
    @tailrec
    def maxTail(list: List[Int], maxValue: Int): Int = list match {
      case Nil => maxValue
      case Const(h, t) if h > maxValue => maxTail(t, h)
      case Const(h, t) if h < maxValue => maxTail(t, maxValue)
      case Const(_, _) => maxValue
    }

    maxTail(list, Int.MinValue)
  }

  //Ejercicio 7
  def min(list: List[Long]): Long = {
    @tailrec
    def minTail(list: List[Long], minValue: Long): Long = list match {
      case Nil => minValue
      case Const(h, t) if h < minValue => minTail(t, h)
      case Const(h, t) if h > minValue => minTail(t, minValue)
      case Const(_, _) => minValue
    }

    minTail(list, Long.MaxValue)
  }

  //Ejercicio 8
  def minMax(list: List[Double]): (Double, Double) = {
    @tailrec
    def minMaxTail(list: List[Double], result: (Double, Double)): (Double, Double) = list match {
      case Nil => result
      case Const(h, t) if h < result._1 && h > result._2 => minMaxTail(t, (h, h))
      case Const(h, t) if h < result._1 && h <= result._2 => minMaxTail(t, (h, result._2))
      case Const(h, t) if h > result._1 && h > result._2 => minMaxTail(t, (result._1, h))
      case Const(h, t) if h > result._1 && h > result._2 => minMaxTail(t, (result._1, result._2))
      case Const(_, _) => result
    }

    minMaxTail(list, (Double.MaxValue, Double.MinValue))
  }

  def length[A](list: List[A]): Int = list match {
    case Nil => 0
    case Const(_, t) => 1 + length(t)
  }

  def addEnd[A](list: List[A], elem: A): List[A] = list match {
    case Nil => Const(elem, Nil)
    case Const(h, t) => Const(h, addEnd(t, elem))
  }

  def append[A](list1: List[A], list2: List[A]): List[A] = (list1, list2) match {
    case (Nil, Nil) => Nil
    case (list1, Nil) => list1
    case (Nil, list2) => list2
    case (Const(h, t), list2) => Const(h, append(t, list2))
  }

  @tailrec
  def drop[A](n: Int, list: List[A]): List[A] = (n, list) match {
    case (0, list) => list
    case (_, Nil) => Nil
    case (n, Const(_, t)) => drop(n - 1, t)
  }

  //Taller 2 Ejercicio 1
  def take[A](n: Int, list: List[A]): List[A] = (n, list) match {
    case (0, _) => Nil
    case (_, Nil) => Nil
    case (n, Const(h, t)) => Const(h, take(n - 1, t))
  }

  //Taller 2 Ejercicio 2
  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Const(_, Nil) => Nil
    case Const(h, t) => Const(h, init(t))
  }

  //Taller 2 Ejercicio 3
  def split[A](list: List[A], n: Int): (List[A], List[A]) = {
    @tailrec
    def splitTail[A](list: List[A], n: Int, accumulator: List[A]): (List[A], List[A]) = (list, n) match {
      case (_, 0) => (accumulator, list)
      case (Nil, _) => (accumulator, list)
      case (Const(h, t), _) => splitTail(t, n - 1, addEnd(accumulator, h))
    }

    splitTail(list, n, Nil)
  }

  //Taller 2 Ejercicio 4
  def zip[A, B](list1: List[A], list2: List[B]): List[(A, B)] = (list1, list2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Const(h, t), Const(h2, t2)) => Const((h, h2), zip(t, t2))
  }

  //Taller 2 Ejercicio 5
  def unzip[A, B](list: List[(A, B)]): (List[A], List[B]) = {
    @tailrec
    def unzipTail[A, B](list: List[(A, B)], newList1: List[A], newList2: List[B]): (List[A], List[B]) =
      list match {
        case Nil => (newList1, newList2)
        case Const((h1, h2), t) => unzipTail(t, addEnd(newList1, h1), addEnd(newList2, h2))
      }

    unzipTail(list, Nil, Nil)
  }

  //Taller 2 Ejercicio 6
  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def reverseTail[A](list: List[A], reversedList: List[A]): List[A] = list match {
      case Nil => reversedList
      case Const(h, t) => reverseTail(t, Const(h, reversedList))
    }

    reverseTail(list, Nil)
  }

  //Taller 2 Ejercicio 7
  def intersperse[A](list: List[A], elem: A): List[A] = list match {
    case Nil => Nil
    case Const(h, Nil) => Const(h, Nil)
    case Const(h, t) => Const(h, Const(elem, intersperse(t, elem)))
  }

  //Taller 2 Ejercicio 8
  def concat[A](list: List[List[A]]): List[A] = {
    @tailrec
    def concatTail[A](list: List[List[A]], accumulator: List[A]): List[A] = list match {
      case Nil => accumulator
      case Const(Nil, outer) => concatTail(outer, accumulator)
      case Const(Const(h, t), outer) => concatTail(Const(t, outer), Const(h, accumulator))
    }

    reverse(concatTail(list, Nil))
  }

  def reduce(list: List[Int], z: Int)(f: (Int, Int) => Int): Int = list match {
    case Nil => z
    case Const(h, t) => f(h, reduce(t, z)(f))
  }

  def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B = list match {
    case Nil => z
    case Const(h, t) => f(h, foldRight(t, z)(f))
  }

  def sumR(list: List[Int]): Int = foldRight(list, 0)((x, y) => x + y)

  def mulR(list: List[Int]): Int = foldRight(list, 0)((x, y) => x * y)

  //Taller 2 Ejercicio 14
  def lengthFold[A](list: List[A]): Int = foldRight(list, 0)((_, y) => 1 + y)

  @tailrec
  def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B): B = list match {
    case Const(h, t) => foldLeft(t, f(z, h))(f)
    case Nil => z
  }

  def sumarUno(list: List[Int]): List[Int] = {
    foldRight(list, Nil: List[Int])((elem, list) => Const(elem + 1, list))
  }

  //Taller 2 Ejercicio 15
  def andF(list: List[Boolean]): Boolean = foldRight(list, true)((x, y) => x && y)

  //Taller 2 Ejercicio 16
  def takeWhile[A](list: List[A])(p: A => Boolean): List[A] =
    foldRight(list, Nil: List[A])((x, y) => if (p(x)) Const(x, y) else Nil)

  //Taller 2 Ejercicio 17
  def filter[A](list: List[A])(p: A => Boolean): List[A] =
    foldRight(list, Nil: List[A])((x, y) => if (p(x)) Const(x, y) else y)

  //Taller 2 Ejercicio 18
  def unzipF[A, B](list: List[(A, B)]): (List[A], List[B]) =
    foldRight(list, (Nil: List[A], Nil: List[B]))((x, y) => (Const(x._1, y._1), Const(x._2, y._2)))

  //Taller 2 Ejercicio 19
  def lengthL[A](list: List[A]): Int = foldLeft(list, 0)((x, _) => 1 + x)

  //Taller 2 Ejercicio 20
  def andL(list: List[Boolean]): Boolean = foldLeft(list, true)((x, y) => x && y)

  //Taller 2 Ejercicio 21
  def takeWhileL[A](list: List[A])(p: A => Boolean): List[A] =
    foldLeft(list, Nil: List[A])((x, y) => if (p(y)) addEnd(x, y) else Nil)

  //Taller 2 Ejercicio 22
  def filterL[A](list: List[A])(p: A => Boolean): List[A] =
    foldLeft(list, Nil: List[A])((x, y) => if (p(y)) addEnd(x, y) else x)

  //Taller 2 Ejercicio 23
  def unzipL[A, B](list: List[(A, B)]): (List[A], List[B]) =
    foldLeft(list, (Nil: List[A], Nil: List[B]))((x, y) => (addEnd(x._1, y._1), addEnd(x._2, y._2)))

  //Reto extra, función comienza con la lista entera para poder implementarse usando foldRight
  def dropWhile[A](list: List[A])(p: A => Boolean): List[A] = {
    def f(a: A, b: (Boolean, List[A])): (Boolean, List[A]) = b match {
      case (true, Const(h, t)) => if (p(h)) (true, t) else (false, Const(h, t))
      case (true, Nil) => (false, Nil)
      case (false, _) => b
    }
    foldRight(list, (true, list))(f)._2
  }
}

