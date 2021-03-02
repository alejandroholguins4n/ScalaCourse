package co.s4n.immutable.tree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TreeTest extends AnyFlatSpec with Matchers {

  "The Tree.size method" should "return the number of branches and leafs" in {
    Tree.size(Branch(Branch(Leaf(10), Leaf(20)), Leaf(30))) shouldEqual 5
    Tree.size(Branch(Leaf(10), Leaf(20))) shouldEqual 3
  }

  "The depth method" should "return the length of the biggest branch" in {
    Tree.depth(Branch(Branch(Leaf(10), Leaf(20)), Leaf(30))) shouldEqual 3
    Tree.depth(Branch(Branch(Leaf(10), Leaf(20)), Branch(Leaf(30), Leaf(40)))) shouldEqual 3
  }
}
