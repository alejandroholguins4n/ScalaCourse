package co.s4n.immutable.classes

class Escuderia(
                 val nombre: String,
                 val conductor: Conductor
               ) {
  def getNombre(): String = {
    this.nombre
  }

  def getConductor(): Conductor = {
    this.conductor
  }
}
