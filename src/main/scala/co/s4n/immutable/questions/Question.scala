package co.s4n.immutable.questions

import scala.annotation.tailrec

class Question {

  //Question 1
  def myLast[A](list: List[A]): A = {
    @tailrec
    def myLastTail[A](list: List[A], last: A): A = list match {
      case Nil => last
      case head :: tail => myLastTail(tail, head)
    }

    myLastTail(list, list.head)
  }

  //Question 2
  def myButLast[A](list: List[A]): A = {
    @tailrec
    def myLastTail[A](list: List[A]): A = list match {
      case head :: _ :: Nil => head
      case _ :: tail => myLastTail(tail)
    }

    myLastTail(list)
  }

  //Question 2.5, should return both the last and second to last element
  def myButLast2[A](list: List[A]): List[A] = {
    @tailrec
    def myLastTail[A](list: List[A]): List[A] = list match {
      case head :: tail :: Nil => List(head, tail)
      case _ :: tail => myLastTail(tail)
    }

    myLastTail(list)
  }

  //Question 3, index starts at 1
  def elementAt[A](list: List[A], position: Int): A = {
    @tailrec
    def elementAtTail[A](list: List[A], position: Int, currentPosition: Int): A = (list, currentPosition) match {
      case (head :: _, 0) => head
      case (_ :: tail, currentPosition) => elementAtTail(tail, position, currentPosition - 1)
    }

    elementAtTail(list, position, position - 1)
  }

  //Question 4
  def myLength[A](list: List[A]): Int = {
    @tailrec
    def myLengthTail[A](list: List[A], counter: Int): Int = (list, counter) match {
      case (Nil, _) => counter
      case (_ :: tail, _) => myLengthTail(tail, counter + 1)
    }

    myLengthTail(list, 0)
  }

  //Question 4.25, implemented with foldRight
  def myLengthFR[A](list: List[A]): Int = list.foldRight(0)((_, y) => y + 1)

  //Question 4.5, implemented with foldLeft
  def myLengthFL[A](list: List[A]): Int = list.foldLeft(0)((y, _) => y + 1)

  //Question 5
  def myReverse[A](list: List[A]): List[A] = {
    @tailrec
    def myReverseTail[A](list: List[A], accumulator: List[A]): List[A] = (list, accumulator) match {
      case (Nil, _) => accumulator
      case (head :: tail, _) => myReverseTail(tail, head :: accumulator)
    }

    myReverseTail(list, List())
  }

  //Question 6
  def isPalindrome[A](list: List[A]): Boolean = list == myReverse(list)

  //Question 8
  def compress[A](list: List[A]): List[A] = {
    @tailrec
    def compressTail[A](list: List[A], accumulator: List[A], lastValue: A): List[A] = (list, accumulator) match {
      case (Nil, _) => accumulator
      case (head :: tail, _) =>
        if (head != lastValue) compressTail(tail, accumulator ::: List(head), head)
        else compressTail(tail, accumulator, head)
    }

    compressTail(list, List(list.head), list.head)
  }

  //Question 9
  def pack[A](list: List[A]): List[Any] = {
    @tailrec
    def packTail[A](list: List[A], accumulator: List[A], lastValue: A): List[Any] = (list, accumulator) match {
      case (Nil, _) => accumulator
      case (head :: tail, List()) => packTail(tail, accumulator ::: List(head), head)
      case (head :: tail, _) =>
        if (head != lastValue) packTail(tail, accumulator ::: List(head), head)
        else packTail(tail, accumulator.init :+ (List(accumulator.last) ::: List(head)), head)
    }

    packTail(list, List(), list.head)
  }

  //Question 10
  def encode[A](list: List[Any]): List[Any] = {
    @tailrec
    def encodeTail[A](list: List[Any], accumulator: List[Any]): List[Any] = list match {
      case Nil => accumulator
      case (head :: tail) :: outerTail => encodeTail(outerTail, accumulator :+ (head, (head :: tail).length))
      case head :: tail => encodeTail(tail, accumulator :+ (head, 1))
    }

    encodeTail(list, List())
  }

  //Question 11
  def encodeModified[A](list: List[Any]): List[Any] = {
    @tailrec
    def encodeTail[A](list: List[Any], accumulator: List[Any]): List[Any] = list match {
      case Nil => accumulator
      case (head :: tail) :: outerTail => encodeTail(outerTail, accumulator :+ (head, (head :: tail).length))
      case head :: tail => encodeTail(tail, accumulator :+ head)
    }

    encodeTail(list, List())
  }

  //Helper function, returns the list with the head element repeated n times
  def repeater[A](value: List[A], occurrences: Int): List[A] = occurrences match {
    case 0 => value.tail
    case _ => repeater(value.head :: value, occurrences - 1)
  }

  //Question 12
  def decodeModified[A](list: List[Any]): List[Any] = {
    @tailrec
    def decodeTail[A](list: List[Any], accumulator: List[Any]): List[Any] = list match {
      case Nil => accumulator
      case (value, occurrences: Int) :: outerTail => decodeTail(outerTail, accumulator :+ repeater(List(value), occurrences))
      case head :: tail => decodeTail(tail, accumulator :+ head)
    }

    decodeTail(list, List())
  }

  //Question 14
  def dupli[A](list: List[A]): List[A] = {
    @tailrec
    def dupliTail[A](list: List[A], accumulator: List[A]): List[A] = (list, accumulator) match {
      case (Nil, _) => accumulator
      case (head :: tail, _) => dupliTail(tail, accumulator ::: List(head, head))
    }

    dupliTail(list, List())
  }

  //Question 15
  def repli[A](list: List[A], occurrences: Int): List[A] = {
    @tailrec
    def repliTail[A](list: List[A], accumulator: List[A]): List[A] = (list, accumulator) match {
      case (Nil, _) => accumulator
      case (head :: tail, _) => repliTail(tail, accumulator ::: repeater(List(head), occurrences))
    }

    repliTail(list, List())
  }

  //Question 16
  def dropEvery[A](list: List[A], toDrop: Int): List[A] = {
    @tailrec
    def dropEveryTail[A](list: List[A], accumulator: List[A], counter: Int): List[A] = (list, counter) match {
      case (Nil, _) => accumulator
      case (_ :: tail, 1) => dropEveryTail(tail, accumulator, toDrop)
      case (head :: tail, _) => dropEveryTail(tail, accumulator :+ head, counter - 1)
    }

    dropEveryTail(list, List(), toDrop)
  }

  //Question 17
  def split[A](list: List[A], splitAt: Int): List[List[A]] = {
    @tailrec
    def splitTail[A](list: List[A], accumulator: List[A], counter: Int): List[List[A]] = (list, counter) match {
      case (_ :: tail, 0) => List(accumulator, tail)
      case (head :: tail, _) => splitTail(tail, accumulator :+ head, counter - 1)
    }

    splitTail(list, List(), splitAt)
  }

  //Question 18, first element exclusive, last element inclusive
  def slice[A](list: List[A], sliceStart: Int, sliceEnd: Int): List[A] = {
    @tailrec
    def splitTail[A](list: List[A], accumulator: List[A], sliceStart: Int, sliceEnd: Int): List[A] =
      (list, sliceStart, sliceEnd) match {
        case (_, _, 0) => accumulator
        case (head :: tail, 1, sliceEnd) => splitTail(tail, accumulator :+ head, 1, sliceEnd - 1)
        case (_ :: tail, sliceStart, sliceEnd) => splitTail(tail, accumulator, sliceStart - 1, sliceEnd - 1)
      }

    splitTail(list, List(), sliceStart, sliceEnd)
  }

  //Question 19, works for both positive and negative numbers, positive left, negative right
  def rotate[A](list: List[A], rotations: Int): List[Any] = {
    @tailrec
    def rotateTail[A](list: List[A], accumulator: List[A], counter: Int): List[Any] = (list, counter) match {
      case (list, 0) =>
        if (rotations > 0) list ::: accumulator
        else accumulator ::: list
      case (head :: tail, counter) =>
        if (counter > 0) rotateTail(tail, accumulator :+ head, counter - 1)
        else rotateTail(head :: tail.init, tail.last :: accumulator, counter + 1)
    }

    rotateTail(list, List(), rotations)
  }

  //Question 20, index starts at 1
  def removeAt[A](list: List[A], position: Int): List[A] = {
    @tailrec
    def removeAt[A](list: List[A], accumulator: List[A], currentPosition: Int): List[A] = (list, currentPosition) match {
      case (_ :: tail, 1) => accumulator ::: tail
      case (head :: tail, currentPosition) => removeAt(tail, accumulator :+ head, currentPosition - 1)
    }

    removeAt(list, List(), position)
  }
}