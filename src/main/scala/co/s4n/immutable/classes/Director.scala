package co.s4n.immutable.classes

class Director(
                val nombre: String,
                val apellido: String,
                val nacimiento: Int
              ) {
  def nombreCompleto: String = s"$nombre $apellido"

  def copy(
            nombre: String = this.nombre,
            apellido: String = this.apellido,
            nacimiento: Int = nacimiento
          ): Director = {
    new Director(nombre, apellido, nacimiento)
  }
}

object Director {
  def apply(nombre: String, apellido: String, nacimiento: Int): Director = {
    new Director(nombre, apellido, nacimiento)
  }

  //Since the birth date is on timestamp, the oldest director is whichever has a lower timestamp
  def esMayor(director1: Director, director2: Director): Director = {
    if (director1.nacimiento < director2.nacimiento) {
      director1
    } else {
      director2
    }
  }
}
