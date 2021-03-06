package co.s4n.immutable.classes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConductorEscuderiaTest extends AnyFlatSpec with Matchers {

  val conductor = new Conductor("Jose", "Perez", 20, 18)
  val escuderia = new Escuderia("Escuderia 1", conductor)

  "The conductor.getApellido method" should "return the last name of Jose" in {
    conductor.getApellido() shouldEqual "Perez"
  }

  "The conductor.getCarrerasNoTerminadas method" should "return his total trips minus the ones he didn't finish" in {
    conductor.getCarrerasNoTerminadas() shouldEqual 2
  }

  "The escuderia.getConductor method" should "return the conductor" in {
    escuderia.getConductor() shouldEqual conductor
  }


}
