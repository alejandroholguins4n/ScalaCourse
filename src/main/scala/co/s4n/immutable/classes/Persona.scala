package co.s4n.immutable.classes

class Persona(val nombre: String, val apellido: String) {
  def nombreCompleto = s"$nombre $apellido"
}

object Persona {
  def apply(nombreCompleto: String): Persona = {
    val splittedName = nombreCompleto.split(" ")
    if (splittedName.length == 4) {
      new Persona(splittedName(0) + " " + splittedName(1), splittedName(2) + " " + splittedName(3))
    } else if (splittedName.length == 3) {
      new Persona(splittedName(0).toString(), splittedName(1) + " " + splittedName(2))
    } else {
      new Persona(splittedName(0).toString(), splittedName(1).toString())
    }
  }
}


