package co.s4n.immutable.classes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PersonaTest extends AnyFlatSpec with Matchers {
  val persona = Persona("Alejandro Holguin")
  val persona2 = Persona("Alejandro Holguin Arango")

  "The Persona.nombre method" should "return the first name" in {
    persona.nombre shouldEqual "Alejandro"
  }

  "The Persona.apellido method" should "return the last name or two last names" in {
    persona2.apellido shouldEqual "Holguin Arango"
  }
}
