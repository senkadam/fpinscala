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

}
