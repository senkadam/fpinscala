package fpinscala.datastructures


import fpinscala.datastructures._
import org.scalatest.Matchers._
import org.scalatest.FunSuite

/**
 * Created by ejdy on 9.12.15.
 */
class ListTest  extends org.scalatest.FlatSpec {

    "Function tail" should "return the list but head" in {
      List.tail(Nil) shouldBe Nil
      List.tail(List(3,4,5,6)) shouldBe List(4,5,6)
      List.tail(List(6)) shouldBe Nil
      List.tail(List("ABC","EFG")) shouldBe List("EFG")



    }

  "Function drop" should "return the list but n first" in {
    List.drop(Nil,3) shouldBe Nil
    List.drop(List(3,4,5,6,7,8),3) shouldBe List(6,7,8)
    List.drop(List(6),-3) shouldBe List(6)
    List.drop(List(6),0) shouldBe List(6)
    List.drop(List(6),1) shouldBe Nil
    List.drop(List("ABC","EFG","XF"),2) shouldBe List("XF")



  }

  "Function dropWhile" should "remove list tail until the function f is true" in {
    List.dropWhile(Nil,(a:Int)=>true) shouldBe Nil
    List.dropWhile(List(3,4,5,6,7,8),(a:Int)=>(a<7)) shouldBe List(7,8)
    List.dropWhile(List(6),(a:Int)=>1<0) shouldBe List(6)
    List.dropWhile(List(6,5,5),(a:Int)=>1>0) shouldBe Nil



  }

  "Function setHead" should "replace a head in a List" in {
    List.setHead(Nil,3) shouldBe List(3)
    List.setHead(List(4,5),3) shouldBe List(3,5)
    List.setHead(List(1),3) shouldBe List(3)
  }

  "Function init" should "return a List but last" in {
    List.init(Nil) shouldBe Nil
    List.init(List(4)) shouldBe Nil
    List.init(List(4,5,6,7)) shouldBe List(4,5,6)
  }

  "Function length" should "return a length of List" in {
    List.length(Nil) shouldBe 0
    List.length(List(4)) shouldBe 1
    List.length(List(4,5,6,7)) shouldBe 4
  }

  "Function sum2" should "return a sum of List" in {
    List.sum2(Nil) shouldBe 0
    List.sum2(List(4)) shouldBe 4
    List.sum2(List(4,5,6,7)) shouldBe 22
  }

  "Function product2" should "return a product of a list" in {
    List.product2(Nil) shouldBe 1
    List.product2(List(4)) shouldBe 4
    List.product2(List(4,5,6,7)) shouldBe 840
  }

  "Function reverse" should "return reversed list" in {
    List.reverse(Nil) shouldBe Nil;
    List.reverse(List(4)) shouldBe List(4)
    List.reverse(List(4, 5, 6, 7)) shouldBe List(7, 6, 5, 4)
  }

  "Function appendFol" should "return appende Lists" in {
    List.appendFold(Nil,Nil) shouldBe Nil;
    List.appendFold(List(4),Nil) shouldBe List(4)
    List.appendFold(Nil,List(5)) shouldBe List(5)
    List.appendFold(List(4, 5, 6, 7),List(1,1)) shouldBe List(4,5,6,7, 1, 1)
  }

  "Function appendList" should "return flatten Lists" in {
    List.appendLists(List(Nil)) shouldBe Nil;
    List.appendLists(List(List(4))) shouldBe List(4)
    List.appendLists(List(List(4),List(5,6),Nil,List(4,3,2))) shouldBe List(4,5,6,4,3,2)

  }


  }
