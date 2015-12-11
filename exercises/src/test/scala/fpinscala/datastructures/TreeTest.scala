package fpinscala.datastructures

/**
 * Created by ejdy on 11.12.15.
 */

import org.scalatest.Matchers._

class TreeTest extends org.scalatest.FlatSpec {

  "Function size" should "compute size of tree" in {
    Tree.size(Leaf(5)) shouldBe 1
    Tree.size(Branch(Leaf(5),Branch(Leaf(7),Branch(Leaf(2),Leaf(19))))) shouldBe 7
    Tree.size(Branch(Leaf(5),Leaf(4))) shouldBe 3



  }

  "Function maximum" should "find max in a tree" in {
    Tree.maximum(Leaf(5)) shouldBe 5
    Tree.maximum(Branch(Leaf(5),Branch(Leaf(7),Branch(Leaf(2),Leaf(19))))) shouldBe 19
    Tree.maximum(Branch(Leaf(5),Branch(Leaf(27),Branch(Leaf(2),Leaf(19))))) shouldBe 27
    Tree.maximum(Branch(Leaf(5),Leaf(4))) shouldBe 5



  }

  "Function depthFold" should "compute depth of tree" in {
    Tree.depthFold(Leaf(5)) shouldBe 1
    Tree.depthFold(Branch(Leaf(5),Branch(Leaf(7),Branch(Leaf(2),Leaf(19))))) shouldBe 4
    Tree.depthFold(Branch(Leaf(5),Leaf(4))) shouldBe 2



  }

  "Function sizeFold" should "compute size of tree" in {
    Tree.sizeFold(Leaf(5)) shouldBe 1
    Tree.sizeFold(Branch(Leaf(5),Branch(Leaf(7),Branch(Leaf(2),Leaf(19))))) shouldBe 7
    Tree.sizeFold(Branch(Leaf(5),Leaf(4))) shouldBe 3



  }

  "Function maximumFold" should "find max in a tree" in {
    Tree.maximumFold(Leaf(5)) shouldBe 5
    Tree.maximum(Branch(Leaf(5),Branch(Leaf(7),Branch(Leaf(2),Leaf(19))))) shouldBe 19
    Tree.maximum(Branch(Leaf(5),Branch(Leaf(27),Branch(Leaf(2),Leaf(19))))) shouldBe 27
    Tree.maximum(Branch(Leaf(5),Leaf(4))) shouldBe 5



  }

  "Function depth" should "compute depth of tree" in {
    Tree.depth(Leaf(5)) shouldBe 1
    Tree.depth(Branch(Leaf(5),Branch(Leaf(7),Branch(Leaf(2),Leaf(19))))) shouldBe 4
    Tree.depth(Branch(Leaf(5),Leaf(4))) shouldBe 2



  }

  "Function map" should "map a function on all elements of a list" in {
    Tree.map(Leaf(5))(_ * 2) shouldBe Leaf(10)
    Tree.map(Branch(Leaf(5),Branch(Leaf(7),Branch(Leaf(2),Leaf(19)))))( _ + 2) shouldBe (Branch(Leaf(7),Branch(Leaf(9),Branch(Leaf(4),Leaf(21)))))
    Tree.map(Branch(Leaf(5),Leaf(4)))( _ - 3) shouldBe Branch(Leaf(2),Leaf(1))



  }


}
