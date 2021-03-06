package co.s4n.immutable.classes

//Taller M4
class Objects {

}

//Ejercicio 1, the double parameter for the "cubo" method was changed to float so it can use the "cuadrado" method
object comp {
  def cuadrado(value: Float): Float = {
    value * value
  }

  def cubo(value: Float): Float = {
    cuadrado(value) * value
  }
}

//Ejercicio 2
object comp2 {
  def cuadrado(value: Long): Long = {
    value * value
  }

  def cubo(value: Long): Long = {
    cuadrado(value) * value
  }
}

//Ejercicio 3, it was only meant to be executed so it doesn't have unit tests
object prueba {
  def x = {
    println("x")
    1
  }

  val y = {
    println("y")
    x + 2
  }

  def z = {
    println("z")
    x
    x + "c"
  }
}