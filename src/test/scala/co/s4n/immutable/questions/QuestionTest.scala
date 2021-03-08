package co.s4n.immutable.questions

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QuestionTest extends AnyFlatSpec with Matchers {

  val question = new Question()

  val listaTest1 = List(1, 2, 3, 4, 5, 6)
  val listaTest2 = List(6, 7, 8, 9, 10, 11, 11, 12)
  val listaTest3 = List(1, 2, 3, 3, 4, 4, 3, 2, 2, 1)
  val listaTest4 = List(1, 2, 3, 4, 3, 2, 1)

  //Problem 1
  "The questions.myLast method" should "return the last element" in {
    question.myLast(listaTest1) shouldEqual 6
  }

  //Problem 2
  "The questions.myButLast method" should "return the second last element" in {
    question.myButLast(listaTest1) shouldEqual 5
  }

  //Problem 3
  "The questions.elementAt method" should "return the element in the index" in {
    question.elementAt(listaTest2, 3) shouldEqual 8
  }

  //Problem 4
  "The questions.myLength method" should "return the length of the list" in {
    question.myLength(listaTest1) shouldEqual 6
  }

  //Problem 5
  "The questions.myReverse method" should "return the second last element" in {
    question.myReverse(listaTest1) shouldEqual List(6, 5, 4, 3, 2, 1)
  }

  //Problem 6
  "The questions.isPalindrome method" should "return true if the list is palindrome" in {
    question.isPalindrome(listaTest4) shouldEqual true
  }

  "The questions.isPalindrome method" should "return false if the list isn't palindrome" in {
    question.isPalindrome(listaTest1) shouldEqual false
  }

  //Problem 8
  "The questions.compress method" should "return the list without consequent repeated elements" in {
    question.compress(listaTest3) shouldEqual List(1, 2, 3, 4, 3, 2, 1)
  }

  //Problem 9
  "The questions.pack method" should "return the list with the consequent repeated elements packed together" in {
    question.pack(listaTest3) shouldEqual List(1, 2, List(3, 3), List(4, 4), 3, List(2, 2), 1)
  }

  //Problem 10
  "The questions.encode method" should "return the list with the elements packed with it's occurrences" in {
    val packedList = question.pack(listaTest3)
    question.encode(packedList) shouldEqual List((1, 1), (2, 1), (3, 2), (4, 2), (3, 1), (2, 2), (1, 1))
  }

  //Problem 11
  "The questions.encodeModified method" should "return the list with the repeated elements packed with it's occurrences" in {
    val packedList = question.pack(listaTest3)
    question.encodeModified(packedList) shouldEqual List(1, 2, (3, 2), (4, 2), 3, (2, 2), 1)
  }

  //Problem 12
  "The questions.decodeModified method" should "return the list with the consequent repeated elements packed together" in {
    val packedList = question.pack(listaTest3)
    val encoded = question.encodeModified(packedList)
    question.decodeModified(encoded) shouldEqual List(1, 2, List(3, 3), List(4, 4), 3, List(2, 2), 1)
  }

  //Problem 14
  "The questions.dupli method" should "return the list with every element duplicated" in {
    question.dupli(listaTest1) shouldEqual List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)
  }

  //Problem 15
  "The questions.repli method" should "return the list with every element replicated n times" in {
    question.repli(listaTest1, 3) shouldEqual List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6)
  }

  //Problem 16
  "The questions.dropEvery method" should "return the list with every n element deleted" in {
    question.dropEvery(listaTest1, 2) shouldEqual List(1, 3, 5)
  }

  //Problem 17
  "The questions.split method" should "return a list of the list split at n" in {
    question.split(listaTest1, 2) shouldEqual List(List(1, 2), List(4, 5, 6))
  }

  //Problem 18
  "The questions.slice method" should "return the values in the list between the two parameters" in {
    question.slice(listaTest1, 2, 4) shouldEqual List(2, 3, 4)
  }

  //Problem 19
  "The questions.rotate method" should "return return the list n times" in {
    question.rotate(listaTest1, 2) shouldEqual List(3, 4, 5, 6, 1, 2)
    question.rotate(listaTest1, -2) shouldEqual List(5, 6, 1, 2, 3, 4)
  }

  //Problem 20
  "The questions.removeAt method" should "return a list without the nth element" in {
    question.removeAt(listaTest1, 2) shouldEqual List(1, 3, 4, 5, 6)
  }
}
