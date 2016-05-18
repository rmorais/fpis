package chapter3

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by rmorais on 18/05/2016.
  */
class TreeTest extends FlatSpec with Matchers {

  "size" should "return 1 given the tree has one leaf" in {
    val tree = Leaf(1)
    Tree.size(tree) should be (1)
  }

  it should "return 2 given the tree has no branches" in {
    val tree = Branch(Leaf(1), Leaf(2))
    Tree.size(tree) should be (2)
  }

  it should "return the size of the given tree" in {
    val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val tree2 = Branch(Branch(Leaf(0), Leaf(1)), Branch(Leaf(2), Leaf(3)))
    Tree.size(tree) should be (5)
    Tree.size(tree2) should be (7)
  }
}
