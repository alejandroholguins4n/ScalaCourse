package co.s4n.immutable.list

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListTest extends AnyFlatSpec with Matchers {

  val testIntList : List[Int] = List(4,15,6)
  val testLongList : List[Long] = List(4222,1555,6222)
  val testTrueList : List[Boolean] = List(true, true, true, true)
  val testMixedList : List[Boolean] = List(true, false, true, true)
  val testFalseList : List[Boolean] = List(false, false, false, false)

  "The List() constructor" should "build a list" in {
    List(1,2,3) shouldEqual Const(1,Const(2,Const(3,Nil)))
  }

  "The List.length method" should "return the list length" in {
    List.length(testIntList) shouldEqual 3
  }

  "The List.sum method" should "return the list members sum" in {
    List.sum(testIntList) shouldEqual 25
  }

  //Ejercicio 2
  "The List.tail method" should "return the list without the first element" in {
    List.tail(testIntList) shouldEqual Const(15, Const(6, Nil))
  }

  //Ejercicio 3
  "The List.head method" should "return the first element of the List" in {
    List.head(testIntList) shouldEqual 4
  }

  //Ejercicio extra
  "The List.removeLastElement method" should "return the List minus the last element" in {
    List.removeLastElement(testIntList) shouldEqual Const(4, Const(15, Nil))
  }

  //Ejercicio 4
  "The List.and method" should "return true with a list full of true values" in {
    List.and(testTrueList) shouldEqual true
  }

  "The List.and method" should "return false with at least one false value in the list" in {
    List.and(testMixedList) shouldEqual false
  }

  //Ejercicio 5
  "The List.or method" should "return false with a list full of false values" in {
    List.or(testFalseList) shouldEqual false
  }

  "The List.or method" should "return true with at least one true value in the list" in {
    List.or(testMixedList) shouldEqual true
  }

  //Ejercicio 6
  "The List.max method" should "return the maximum value of the List" in {
    List.max(testIntList) shouldEqual 15
  }

  //Ejercicio 7
  "The List.min method" should "return the minimum value of the List" in {
    List.min(testLongList) shouldEqual 1555
  }
}
