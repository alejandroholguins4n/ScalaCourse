package co.s4n.immutable.traits

sealed trait Forma {
  def tamaño(): Int

  def perimetro(): Double

  def area(): Double
}

trait Rectangular extends Forma {
  def ladoA: Int

  def ladoB: Int
}

class Circulo(val radio: Double) extends Forma {

  //A circle doesn't have sides
  override def tamaño(): Int = 0

  override def perimetro(): Double = 2 * math.Pi * radio

  override def area(): Double = math.Pi * radio * radio
}

class Cuadrado(val lado: Int) extends Rectangular {
  override def ladoA: Int = lado

  override def ladoB: Int = lado

  override def tamaño(): Int = 4

  override def perimetro(): Double = lado * 4

  override def area(): Double = lado * lado
}

class Rectangulo(val ladoA: Int, val ladoB: Int) extends Rectangular {
  override def tamaño(): Int = 4

  override def perimetro(): Double = (ladoA * 2) + (ladoB * 2)

  override def area(): Double = ladoA * ladoB
}

object Draw {
  def apply(forma: Forma): String = forma match {
    case _: Circulo => "El radio del círculo es " + forma.asInstanceOf[Circulo].radio.toString
    case _: Rectangular => "El rectangulo tiene un largo de " +
      forma.asInstanceOf[Rectangular].ladoA.toString +
      "y ancho " +
      forma.asInstanceOf[Rectangular].ladoB.toString
  }
}


