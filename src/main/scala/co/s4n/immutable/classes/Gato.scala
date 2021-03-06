package co.s4n.immutable.classes

class Gato(val nombre: String, val color: String, val comida: String) {}

//The cats were created in the unit tests

object ventaDeChurrus {
  def despachar(gato: Gato): Boolean = gato.comida match {
    case "Churrus" => true
    case _ => false
  }
}