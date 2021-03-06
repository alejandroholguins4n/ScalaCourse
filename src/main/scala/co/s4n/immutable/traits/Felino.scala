package co.s4n.immutable.traits

trait Felino {
  def color: String

  def sonido: String
}

case class Leon(val color: String, val sonido: String, val tamañoMelena: Int) extends Felino {

}

class Tigre(val color: String, val sonido: String) extends Felino {

}

class Jaguar(val color: String, val sonido: String) extends Felino {

}

class Gato(val color: String, val sonido: String, val comidaFavorita: String) extends Felino {

}