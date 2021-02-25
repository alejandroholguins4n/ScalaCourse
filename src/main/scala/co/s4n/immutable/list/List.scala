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

  def head[A](lst : List[A]) = lst match {
    case Nil => Nil
    case Const(h, t) => h
  }

  def tail[A](lst : List[A]) : List[A] = lst match {
    case Const(_, Nil) => Nil
    case Const(h, t) => Const(h, tail(t))
  }

  def length[A](lst : List[A]) : Int = lst match {
    case Nil => 0
    case Const(h, t) => 1 + length(t)
  }
}

