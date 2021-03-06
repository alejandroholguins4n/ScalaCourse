package co.s4n.immutable.traits

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FormaTest extends AnyFlatSpec with Matchers {

  val circulo = new Circulo(10)
  val cuadrado = new Cuadrado(5)
  val rectangulo = new Rectangulo(5, 7)

  "The Circulo.perimetro method" should "return the perimeter of the figure" in {
    circulo.perimetro() shouldEqual 62.83 +- 1
  }

  "The Cuadrado.area method" should "return the area of the figure" in {
    cuadrado.area() shouldEqual 25
  }

  "The Rectangulo.tamaño method" should "return the number of sides of the figure" in {
    rectangulo.tamaño() shouldEqual 4
  }

  "The Draw(Circulo) method" should "return a string with it's radius" in {
    Draw(circulo) shouldEqual "El radio del círculo es 10.0"
  }

  "The Draw(Rectangular) method" should "return a string with it's sides" in {
    Draw(cuadrado) shouldEqual "El rectangulo tiene un largo de 5y ancho 5"
  }

}
