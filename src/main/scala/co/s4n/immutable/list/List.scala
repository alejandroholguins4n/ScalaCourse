package co.s4n.immutable.list

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Const[+A](h : A, t : List[A]) extends List[A]

object List {

  def apply[A](as: A*) : List[A] = {
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail : _*))
  }

  def sum(ints : List[Int]) : Int = ints match {
    case Nil => 0
    case Const(h, t) => h + sum(t)
  }

  //Ejercicio 2
  def tail[A](lst : List[A]) : List[A] = lst match {
    case Nil => Nil
    case Const(h, t) => t
  }

  //Ejercicio 3
  def head[A](lst : List[A]) : Any  = lst match {
    case Nil => Nil
    case Const(h, t) => h
  }

  //Ejercicio extra
  def removeLastElement[A](lst : List[A]) : List[A] = lst match {
    case Nil => Nil
    case Const(_, Nil) => Nil
    case Const(h, t) => Const(h, removeLastElement(t))
  }

  //Ejercicio 4
  def and(lst : List[Boolean]) : Boolean = lst match{
    case Nil => false
    case Const(false, Nil) => false
    case Const(false, t) => false
    case Const(true, Nil) => true
    case Const(true, t) => and(t)
  }

  //Ejercicio 5
  def or(lst : List[Boolean]) : Boolean = lst match{
    case Nil => false
    case Const(false, Nil) => false
    case Const(false, t) => and(t)
    case Const(true, Nil) => true
    case Const(true, t) => true
  }

  //Ejercicio 6
  def max(lst : List[Int]) : Int = {
    def maxTail(lst : List[Int], maxValue : Int) : Int = lst match {
      case Nil => maxValue
      case Const(h, t) if h > maxValue => maxTail(t, h)
      case Const(h, t) if h < maxValue => maxTail(t, maxValue)
      case Const(_, _) => maxValue
    }
    maxTail(lst, Int.MinValue)
  }

  //Ejercicio 7
  def min(lst : List[Long]) : Long = {
    def minTail(lst : List[Long], minValue : Long) : Long = lst match {
      case Nil => minValue
      case Const(h, t) if h < minValue => minTail(t, h)
      case Const(h, t) if h > minValue => minTail(t, minValue)
      case Const(_, _) => minValue
    }
    minTail(lst, Long.MaxValue)
  }
  //Ejercicio 8
  def minMax(lst : List[Double]) : (Double, Double) = {
    def minMaxTail(lst : List[Double], result : (Double, Double)) : (Double, Double) = lst match {
      case Nil => result
      case Const(h, t) if h < result._1 && h > result._2 => minMaxTail(t,(h, h))
      case Const(h, t) if h < result._1 && h <= result._2 => minMaxTail(t,(h, result._2))
      case Const(h, t) if h > result._1 && h > result._2 => minMaxTail(t,(result._1, h))
      case Const(h, t) if h > result._1 && h > result._2 => minMaxTail(t,(result._1, result._2))
      case Const(_, _) => result
    }
    minMaxTail(lst,(Double.MaxValue, Double.MinValue))
  }

  def length[A](lst : List[A]) : Int = lst match {
    case Nil => 0
    case Const(h, t) => 1 + length(t)
  }
}

