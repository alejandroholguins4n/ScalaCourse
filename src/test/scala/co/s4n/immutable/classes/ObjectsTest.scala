package co.s4n.immutable.classes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ObjectsTest extends AnyFlatSpec with Matchers {

  //Ejercicio 1
  "The comp.cuadrado method" should "return the number 25" in {
    comp.cuadrado(5) shouldEqual 25.0
  }

  "The comp.cuadrado method" should "return the number 81" in {
    comp.cuadrado(9) shouldEqual 81.0
  }

  "The comp.cubo method" should "return the cube of the number 5" in {
    comp.cubo(5) shouldEqual 125.0
  }

  "The comp.cubo method" should "return the cube of the number 9" in {
    comp.cubo(9) shouldEqual 729.0
  }

  //Ejercicio 2
  "The comp2.cuadrado method" should "return the number 25" in {
    comp2.cuadrado(5) shouldEqual 25
  }

  "The comp2.cuadrado method" should "return the number 81" in {
    comp2.cuadrado(9) shouldEqual 81
  }

  "The comp2.cubo method" should "return the cube of the number 5" in {
    comp2.cubo(5) shouldEqual 125
  }

  "The comp2.cubo method" should "return the cube of the number 9" in {
    comp2.cubo(9) shouldEqual 729
  }
}
