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

}
