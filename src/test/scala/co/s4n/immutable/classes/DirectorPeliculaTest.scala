package co.s4n.immutable.classes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DirectorPeliculaTest extends AnyFlatSpec with Matchers {

  val director1 = Director("Peranito", "Perez", 188193601)
  val director2 = Director("Argemirito", "Lopez", 661579201)

  var pelicula1 = Pelicula("Mr. Nobody", 2009, 7.8, director1)
  var pelicula2 = Pelicula("Parasite", 2019, 8.6, director2)

  "The Director.esMayor method" should "return the oldest director" in {
    Director.esMayor(director1, director2) shouldEqual director1
  }

  "The conductor.mayorDirectorEnElTiempo method" should "return the oldest director when the movie aired" in {
    Pelicula.mayorDirectorEnElTiempo(pelicula1, pelicula2) shouldEqual director1
  }

  "The Pelicula.mejorCalificada method" should "return the best rated movie" in {
    Pelicula.mejorCalificada(pelicula1, pelicula2) shouldEqual pelicula2
  }


}
