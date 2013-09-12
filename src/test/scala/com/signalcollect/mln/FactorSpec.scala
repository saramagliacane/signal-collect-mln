package com.signalcollect.mln

import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner
import Distribution._

@RunWith(classOf[JUnitRunner])
class FactorSpec extends SpecificationWithJUnit with Serializable {

  sequential

  "Factor" should {
    "normalize correctly for one value" in {
      val f = new Factor().addValue("a", 0.4)
      f("a") === 0.4
      val fNormalized = f.normalize
      fNormalized("a") === 1.0
    }

    "normalize correctly for two values" in {
      val f = new Factor().addValue("a", 0.4).addValue("b", 0.1)
      f("a") === 0.4
      val fNormalized = f.normalize
      fNormalized("a") === 0.8
    }

  }
}