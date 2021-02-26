package co.s4n.immutable.nat

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NatTest extends AnyFlatSpec with Matchers {

  val four : Nat = Suc(Suc(Suc(Suc(Cero))))

  //Ejercicio 10
  "The fromNatToInt method" should "return the number represented" in {
    Nat.fromNatToInt(four) shouldEqual 4
  }
  "The fromNatToInt method with a cero" should "return 0" in {
    Nat.fromNatToInt(Cero) shouldEqual 0
  }

  //Ejercicio 11
  "The fromIntToNat method" should "return the Nat value" in {
    Nat.fromIntToNat(4) shouldEqual four
  }

  "The fromIntToNat method with 0" should "return Cero" in {
    Nat.fromIntToNat(0) shouldEqual Cero
  }
}
