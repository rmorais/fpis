package chapter3

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by rmorais on 18/05/2016.
  */
class TreeTest extends FlatSpec with Matchers {

  "size" should "return 1 given the tree has one leaf" in {
    val tree = Leaf(1)
    Tree.size(tree) should be (1)
    Tree.sizeViaFold(tree) should be (1)
  }

  it should "return 2 given the tree has no branches" in {
    val tree = Branch(Leaf(1), Leaf(2))
    Tree.size(tree) should be (3)
    Tree.sizeViaFold(tree) should be (3)
  }

  it should "return the size of the given tree" in {
    val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val tree2 = Branch(Branch(Leaf(0), Leaf(1)), Branch(Leaf(2), Leaf(3)))
    Tree.size(tree) should be (5)
    Tree.size(tree2) should be (7)
    Tree.sizeViaFold(tree) should be (5)
    Tree.sizeViaFold(tree2) should be (7)
  }

  "maximum" should "return the maximum value of the tree" in {
    Tree.maximum(Leaf(1)) should be(1)
    Tree.maximum(Branch(Leaf(1), Leaf(2))) should be(2)
    Tree.maximum(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))) should be(3)
    Tree.maximum(Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(2), Leaf(100)))) should be(100)
    Tree.maximumViaFold(Leaf(1)) should be(1)
    Tree.maximumViaFold(Branch(Leaf(1), Leaf(2))) should be(2)
    Tree.maximumViaFold(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))) should be(3)
    Tree.maximumViaFold(Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(2), Leaf(100)))) should be(100)
  }

  "depth" should "return 0 given the tree only has one leaf" in {
    val tree = Leaf(1)
    Tree.depth(tree) should be(0)
    Tree.depthViaFold(tree) should be(0)
   }

  it should "return the depth of the tree" in {
    Tree.depth(Branch(Leaf(1), Leaf(2))) should be(1)
    Tree.depth(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))) should be(2)
    Tree.depth(Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(2), Leaf(100)))) should be(2)
    Tree.depth(Branch(Branch(Branch(Leaf(1), Leaf(3)), Leaf(3)), Branch(Leaf(2), Leaf(100)))) should be(3)
    Tree.depthViaFold(Branch(Leaf(1), Leaf(2))) should be(1)
    Tree.depthViaFold(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))) should be(2)
    Tree.depthViaFold(Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(2), Leaf(100)))) should be(2)
    Tree.depthViaFold(Branch(Branch(Branch(Leaf(1), Leaf(3)), Leaf(3)), Branch(Leaf(2), Leaf(100)))) should be(3)
  }
}
