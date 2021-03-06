package co.s4n.immutable.traits

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ColorTest extends AnyFlatSpec with Matchers {

  val rojo = Color("Rojo")

  "The Color(predefined) method" should "return the predifined color" in {
    rojo.red shouldEqual 255
    rojo.green shouldEqual 0
    rojo.blue shouldEqual 0
  }

  "The Color(Int, Int, Int) method" should "return the custom color" in {
    Color(128,220,25).red shouldEqual 128
  }
}
