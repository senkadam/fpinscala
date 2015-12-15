package fpinscala.errorhandling

import java.util.regex.Pattern

import fpinscala.errorhandling
import org.scalatest.FunSuite
import org.scalatest.Matchers._

/**
 * Created by ejdy on 14.12.15.
 */
class OptionTest extends org.scalatest.FlatSpec {

"Option map" should "return the same for my and library implementation" in{
  scala.Some(5).map(_+1).getOrElse(-1) shouldBe errorhandling.Some(5).map(_+1).getOrElse(-1)
  scala.None.map((a:Int)=> a+1).getOrElse(-1) shouldBe errorhandling.None.map((a:Int)=> a+1).getOrElse(-1)

}

  "Option filter" should "return the same for my and library implementation" in{
    scala.Some(5).filter(_>3).getOrElse(-1) shouldBe errorhandling.Some(5).filter(_>3).getOrElse(-1)
    scala.Some(1).filter( _ > 3).getOrElse(-1) shouldBe errorhandling.Some(1).filter(_ > 3).getOrElse(-1)

    scala.None.filter((a:Int)=> a>1).getOrElse(-1) shouldBe errorhandling.None.filter((a:Int)=> a>1).getOrElse(-1)

  }


  "Option flatMat" should "return the same for my and library implementation" in{
    scala.List(3,4,5).find((a)=>(a==5)).flatMap((a:Int)=>scala.Some(4))getOrElse(-1) shouldBe
      errorhandling.Some(5).flatMap((a:Int)=>errorhandling.Some(4)).getOrElse(-1)
    scala.List(3,4,5).find((a)=>(a==6)).flatMap((a:Int)=>scala.Some(4))getOrElse(-1) shouldBe
      errorhandling.None.flatMap((a:Int)=>errorhandling.Some(4)).getOrElse(-1)

 }

  "Option getOrElse" should "return the same for my and library implementation" in{
    scala.Some(5).getOrElse(1) shouldBe errorhandling.Some(5).getOrElse(1)
    scala.None.getOrElse(1) shouldBe errorhandling.None.getOrElse(1)

  }

  "Option orElse" should "return the same for my and library implementation" in{
    scala.Some(5).orElse(scala.Some(1)).getOrElse(-1) shouldBe errorhandling.Some(5).orElse(Some(1)).getOrElse(-1)
    scala.None.orElse(scala.Some(1)).getOrElse(-1) shouldBe errorhandling.None.orElse(Some(1)).getOrElse(-1)

  }

  "variance of list" should "be equal to" in {
    Option.variance(List(4,5,6)) shouldBe Some(2.0/3.0)
    Option.variance(List(0)) shouldBe Some(0.0)
    Option.variance(List()) shouldBe None
  }

  "bothMath" should "math both patterns" in {
    Option.bothMatch("ba+","ba*","baaaaaaaaaaa") shouldBe Some(true)
    Option.bothMatch("ba+","ba*","bbbbbbbbbbbbbb") shouldBe Some(false)
    Option.bothMatch("ba+","ba*","b") shouldBe Some(false)
    Option.bothMatch("ba+","+","b") shouldBe None
    Option.bothMatch("telef(on|","ba*","b") shouldBe None
  }

  "sequence" should "transform List of Some to Some list" in {
    Option.sequence(List(Some(3),Some(4),Some(5))) shouldBe Some(List(3,4,5))
    Option.sequence(List(Some(3),Some(4),Some(5),None)) shouldBe None
    Option.sequence(List(Some(3))) shouldBe Some(List(3))
    Option.sequence(List(None)) shouldBe None
    Option.sequence(List()) shouldBe Some(List())
    Option.sequence(List(Some(3),Some(4),None,Some(5))) shouldBe None
    Option.sequence(List(None,None,None)) shouldBe None
  }


  "traverse" should "return Some of list" in {
    Option.traverse(List("ab+"))(Option.pattern(_)).getOrElse(List())(0).pattern() shouldEqual  "ab+"
    Option.traverse(List("ab+","bc*"))(Option.pattern(_)).getOrElse(List())(0).pattern() shouldBe "ab+"

    Option.traverse(List("+"))(Option.pattern(_)) shouldBe None
    Option.traverse(List("ab+","*"))(Option.pattern(_)) shouldBe None
  }








}
