package chapter4

import org.scalatest.{MustMatchers, WordSpec}
import chapter4.FunctionsWithOptions._

class Chapter4Spec extends WordSpec with MustMatchers {
  "Chaptser4" should {

    "isDefined: None->false; Some->true" in {
      var option: Option[Int] = None
      option.isDefined mustBe false

      option = Some(1)
      option.isDefined mustBe true
    }

    "mean(1.0, 2.0, 3.0) => 2.0" in {
      mean(List(1.0, 2.0, 3.0)) mustBe Some(2.0)
    }

    "variance(1.0, 2.0, 3.0) => 0.6(6)" in {
      val v = variance(List(1.0, 2.0, 3.0))
      assert(v.isDefined)
      val vg = v.get
      assert(vg >= 0.66 && vg < 0.67)
    }

  }
}