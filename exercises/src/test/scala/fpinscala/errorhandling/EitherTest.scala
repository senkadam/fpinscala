package fpinscala.errorhandling

import org.scalatest.Matchers._
import fpinscala.errorhandling

/**
 * Created by ejdy on 31.12.15.
 */
class EitherTest extends org.scalatest.FlatSpec {

"Either map" should "be aplied only on right" in{
Right(5).map(_ + 1) shouldBe Right(6)
Left(3).map((a:Int)=>a + 1) shouldBe Left(3) ;

}



}
