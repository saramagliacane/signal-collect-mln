package com.signalcollect.bp

import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner

import Distribution._

import org.specs2.matcher.Matcher

@RunWith(classOf[JUnitRunner])
class FactorSpec extends SpecificationWithJUnit with Serializable {

  sequential

  "Factor" should {

    val fA1 = Factor() + ("a", 0.4)
    val fA2 = Factor() + ("a", 0.1)
    val fB = Factor() + ("b", 0.1)

    "normalize correctly for one value" in {
      fA1.normalize("a") must beApproximately(1.0)
    }

    "normalize correctly for two values" in {
      val fAB = fA1 + fB
      fAB.normalize("a") must beApproximately(0.8)
    }

    "support addition" in {
      val fAadded = fA1 + fA2
      fAadded("a") must beApproximately(0.5)
    }

    "support subtraction" in {
      val fAsubtracted = fA1 - fA2
      fAsubtracted("a") must beApproximately(0.3)
    }

    "support multiplication" in {
      val fAmultiplied = fA1 * fA2
      fAmultiplied("a") must beApproximately(0.04)
    }

    "support division" in {
      val fAmultiplied = fA1 * fA2
      val shouldBeA1 = fAmultiplied / fA2
      val shouldBeA2 = fAmultiplied / fA1
      shouldBeA1("a") must beApproximately(fA1("a"))
      shouldBeA2("a") must beApproximately(fA2("a"))
    }

    //TODO: Add tests for logic stuff.
  }

  def beApproximately(x: Double) = beCloseTo(x, 0.000000001)

}
