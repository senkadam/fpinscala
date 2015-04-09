package fpinscala.gettingstarted

import org.scalatest.Matchers._

/**
 * Created by senk on 8.4.15.
 */
class PolymorphicFunctionsTest extends org.scalatest.FlatSpec {

  "Function is sorte" should "be polimorfic" in {
    PolymorphicFunctions.isSorted[Int](Array(1,3,4),_ < _) shouldBe true

    PolymorphicFunctions.isSorted[Int](Array(1,5,4),_ < _) shouldBe false

    PolymorphicFunctions.isSorted[Int](Array(6,5,4),_ < _) shouldBe false

    PolymorphicFunctions.isSorted[Int](Array(6,5,4),_ > _) shouldBe true

    PolymorphicFunctions.isSorted[Int](Array(6),_ < _) shouldBe true


  }


}
