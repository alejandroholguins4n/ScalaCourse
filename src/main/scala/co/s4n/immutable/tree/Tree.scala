package co.s4n.immutable.tree

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(value) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def depth[A](tree: Tree[A]): Int = {
    def depthCounter[A](tree: Tree[A], accum: Int): Int = tree match {
      case Leaf(_) => accum
      case Branch(left, right) => math.max(depthCounter(right, accum + 1), depthCounter(left, accum + 1))
    }

    depthCounter(tree, 1)
  }
}
