package co.s4n.immutable.classes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GatoTest extends AnyFlatSpec with Matchers {

  val gato1 = new Gato("IO", "Fawn", "Churrus")
  val gato2 = new Gato("Maker", "Red", "Leche")
  val gato3 = new Gato("Docker", "Blue", "Cuido")

  "The ventaDeChurrus.despachar method" should "return true when the cat likes churrus" in {
    ventaDeChurrus.despachar(gato1) shouldEqual true
  }

  "The comp.cuadrado method" should "return false when the cat doesn't" in {
    ventaDeChurrus.despachar(gato2) shouldEqual false
  }
}
