package co.s4n.immutable.classes

class Conductor(
                 val nombre: String,
                 val apellido: String,
                 val totalCarreras: Int,
                 val carrerasTerminadas: Int
               ) {

  def getNombre(): String = {
    this.nombre
  }

  def getApellido(): String = {
    this.apellido
  }

  def getTotalCarreras(): Int = {
    this.totalCarreras
  }

  def getCarrerasTerminadas(): Int = {
    this.carrerasTerminadas
  }

  def getCarrerasNoTerminadas(): Int = {
    this.totalCarreras - this.carrerasTerminadas
  }
}

