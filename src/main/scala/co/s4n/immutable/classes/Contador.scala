package co.s4n.immutable.classes

class Contador(val value: Int) {

  def incr(): Contador = {
    new Contador(value + 1)
  }

  //Was created as a second function so incr with 1 as increment can also be called without parenthesis
  def incr(increment: Int = 1): Contador = {
    new Contador(value + increment)
  }

  def decr(): Contador = {
    new Contador(value - 1)
  }

  def decr(decrement: Int = 1): Contador = {
    new Contador(value - decrement)
  }

  def ajuste(sumador: Sumador): Contador = {
    new Contador(sumador.adicionar(this.value))
  }
}
