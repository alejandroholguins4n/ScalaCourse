package co.s4n.immutable.nat

sealed trait Nat
case object Cero extends Nat
case class Suc(nat : Nat) extends Nat

object Nat {

  //Ejercicio 10
  def fromNatToInt(nat : Nat) : Int = {
    def fromNatToIntTail(nat : Nat, accum : Int) : Int = nat match {
      case Cero => accum
      case Suc(x) => fromNatToIntTail(x, accum + 1)
    }
    fromNatToIntTail(nat, 0)
  }

  //Ejercicio 11
  def fromIntToNat(int : Int) : Nat = {
    def fromIntToNatTail(int : Int, accum : Nat) : Nat = int match {
      case 0 => accum
      case x => fromIntToNatTail(x - 1, Suc(accum))
    }
    fromIntToNatTail(int, Cero)
  }
}
