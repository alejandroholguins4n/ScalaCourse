package co.s4n.immutable.classes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ContadorTest extends AnyFlatSpec with Matchers {

  "The Contador.incr and Contador.decr method" should "return a Contador with its value increased or decreased" in {
    new Contador(10).incr.decr.incr.incr.value shouldEqual new Contador(12).value
    new Contador(10).incr(5).decr(2).incr.incr.value shouldEqual new Contador(15).value
  }

  "The Contador.ajuste" should "return a counter adjusted with the value of the sumador" in {
    val sumador = new Sumador(5)
    new Contador(10).ajuste(sumador).value shouldEqual new Contador(15).value
  }
}
