package co.s4n.immutable.nat

import scala.annotation.tailrec

sealed trait Nat

case object Cero extends Nat

case class Suc(nat: Nat) extends Nat

object Nat {

  //Ejercicio 10
  def fromNatToInt(nat: Nat): Int = {
    @tailrec
    def fromNatToIntTail(nat: Nat, accum: Int): Int = nat match {
      case Cero => accum
      case Suc(x) => fromNatToIntTail(x, accum + 1)
    }

    fromNatToIntTail(nat, 0)
  }

  //Ejercicio 11
  def fromIntToNat(int: Int): Nat = {
    @tailrec
    def fromIntToNatTail(int: Int, accum: Nat): Nat = int match {
      case 0 => accum
      case x => fromIntToNatTail(x - 1, Suc(accum))
    }

    fromIntToNatTail(int, Cero)
  }

  //Taller 2 Ejercicio 9
  def addNat(nat1: Nat, nat2: Nat): Nat = {
    def addNatTail(nat1: Nat, nat2: Nat): Nat = (nat1, nat2) match {
      case (Cero, x) => x
      case (Suc(n), x) => Suc(addNatTail(n, x))
    }

    addNatTail(nat1, nat2)
  }

  //Taller2 Ejercicio 10
  def prodNat(nat1: Nat, nat2: Nat): Nat = {
    @tailrec
    def prodNatTail(nat1: Nat, nat2: Nat, accum: Nat): Nat = (nat1, nat2) match {
      case (Cero, _) => accum
      case (Suc(n), x) => prodNatTail(n, x, addNat(x, accum))
    }

    prodNatTail(nat1, nat2, Cero)
  }
}
