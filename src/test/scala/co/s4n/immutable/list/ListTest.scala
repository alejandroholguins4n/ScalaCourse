package co.s4n.immutable.list

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListTest extends AnyFlatSpec with Matchers {

  val testIntList: List[Int] = List(4, 15, 6)
  val testLongList: List[Long] = List(4222, 1555, 6222)
  val testTrueList: List[Boolean] = List(true, true, true, true)
  val testMixedList: List[Boolean] = List(true, false, true, true)
  val testFalseList: List[Boolean] = List(false, false, false, false)
  val testDoubleList: List[Double] = List(5.0, 6.6, 16.8, 2.1, 66684.2)

  "The List() constructor" should "build a list" in {
    List(1, 2, 3) shouldEqual Const(1, Const(2, Const(3, Nil)))
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

  //Ejercicio 8
  "The List.minMax method" should "return the minimum value of the List" in {
    List.minMax(testDoubleList) shouldEqual(2.1, 66684.2)
  }

  //Taller 2 Ejercicio 1
  "The List.take method" should "return the first n elements of the list" in {
    List.take(3, List("a", "b", "c", "d", "e")) shouldEqual List("a", "b", "c")
  }

  "The List.take method" should "return nil when the first parameter is 0" in {
    List.take(0, List(1, 2, 3, 4)) shouldEqual Nil
  }

  //Taller 2 Ejercicio 2
  "The List.init method" should "return the List minus the last element" in {
    List.init(testIntList) shouldEqual Const(4, Const(15, Nil))
  }

  "The List.init method" should "return Nil when there is only one parameter" in {
    List.init(List(1)) shouldEqual Nil
  }

  //Taller 2 Ejercicio 3
  "The List.split method" should "return two lists separated at the first parameter" in {
    List.split(List(1, 2, 3, 4, 5, 6, 7), 3) shouldEqual(List(1, 2, 3), List(4, 5, 6, 7))
  }

  "The List.split method" should "return nil along the entire list when 0 is the first parameter" in {
    List.split(List(1, 2, 3, 4, 5, 6, 7), 0) shouldEqual(Nil, List(1, 2, 3, 4, 5, 6, 7))
  }

  //Taller 2 Ejercicio 4
  "The List.zip method" should "return a single list with both lists stacked one item at the time" in {
    List.zip(List(1, 2, 3), List(true, false, true, true)) shouldEqual List((1, true), (2, false), (3, true))
  }

  //Taller 2 Ejercicio 5
  "The List.unzip method" should "return both lists previously zipped" in {
    List.unzip(List((1, "a"), (2, "b"), (3, "c"))) shouldEqual(List(1, 2, 3), List("a", "b", "c"))
  }

  //Taller 2 Ejercicio 6
  "The List.reverse method" should "return the same list in inverse order" in {
    List.reverse(List(1, 2, 3, 4)) shouldEqual List(4, 3, 2, 1)
  }

  //Taller 2 Ejercicio 7
  "The List.intersperse method" should "return the list with the parameter inserted in between each position" in {
    List.intersperse(List(2, 3, 4, 5), 1) shouldEqual List(2, 1, 3, 1, 4, 1, 5)
  }

  //Taller 2 Ejercicio 8
  "The List.concat method" should "return a single list with all the values" in {
    List.concat(List(List(1, 2, 3), List(4, 5, 6))) shouldEqual List(1, 2, 3, 4, 5, 6)
  }

  "The List.concat method" should "return a single list with the values of all the lists" in {
    List.concat(List(List(1.0, 2.0), Nil, List(3.0, 4.0))) shouldEqual List(1.0, 2.0, 3.0, 4.0)
  }

  //Taller 2 Ejercicio 15
  "The List.andF" should "return false when there is at least one false value" in {
    List.andF(testMixedList) shouldEqual false
  }

  //Taller 2 Ejercicio 16
  "The List.takeWhile" should "return the longest sub-list that complies the function" in {
    List.takeWhile(List(true, true, false, true))((x) => if (x) true else false) shouldEqual Const(true, Const(true, Nil))
  }

  //Taller 2 Ejercicio 17
  "The List.filter method" should "return a list with every value that meets the function" in {
    List.filter(List(true, true, false, true))((x) => if (x) true else false) shouldEqual Const(true, Const(true, Const(true, Nil)))
  }

  //Taller 2 Ejercicio 18
  "The List.unzipF method" should "return both lists previously zipped" in {
    List.unzipF(List((1, "a"), (2, "b"), (3, "c"))) shouldEqual(List(1, 2, 3), List("a", "b", "c"))
  }

  //Taller 2 Ejercicio 19
  "The List.lengthL method" should "return the length of the list" in {
    List.lengthL(List(true, true, false, true)) shouldEqual 4
  }

  //Taller 2 Ejercicio 20
  "The List.andL" should "return false when there is at least one false value" in {
    List.andL(testMixedList) shouldEqual false
  }

  //Taller 2 Ejercicio 21
  "The List.takeWhileL" should "return the longest list that complies the function" in {
    List.takeWhileL(List(true, true, false, true))((x) => if (x) true else false) shouldEqual Const(true, Nil)
  }

  //Taller 2 Ejercicio 22
  "The List.filterL method" should "return a list with every value that meets the function" in {
    List.filterL(List(true, true, false, true))((x) => if (x) true else false) shouldEqual Const(true, Const(true, Const(true, Nil)))
    //List.filterL(List(1, 2, 3, 4, 5))(_ > 3) shouldEqual Const(true, Const(true, Const(true, Nil)))
  }

  //Taller 2 Ejercicio 23
  "The List.unzipL method" should "return both lists previously zipped" in {
    List.unzipL(List((1, "a"), (2, "b"), (3, "c"))) shouldEqual(List(1, 2, 3), List("a", "b", "c"))
  }
}
