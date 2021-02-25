package co.s4n.immutable.list

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListTest extends AnyFlatSpec with Matchers {
  "The List() constructor" should "build a list" in {
    List(1,2,3) shouldEqual Const(1,Const(2,Const(3,Nil)))
  }
  val testIntList = List(4,5,6)
  "The List.length method" should "return the list length" in {
    List.length(testIntList) shouldEqual 3
  }

  "The List.sum method" should "return the list members sum" in {
    List.sum(testIntList) shouldEqual 15
  }

  "The List.head method" should "return the first element of the List" in {
    List.head(testIntList) shouldEqual 4
  }

  "The List.tail method" should "return the List minus the last element" in {
    List.tail(testIntList) shouldEqual Const(4, Const(5, Nil))
  }


}
