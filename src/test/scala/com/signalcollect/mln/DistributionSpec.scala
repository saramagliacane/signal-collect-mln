package com.signalcollect.mln

import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner
import Distribution._

@RunWith(classOf[JUnitRunner])
class DistributionSpec extends SpecificationWithJUnit with Serializable {

  sequential

  "Distribution" should {

    val distX = bernoulli("x", 0.6)
    val distY = bernoulli("y", 0.9)
    val xTrue = "x" boundTo true
    val xFalse = "x" boundTo false
    val yTrue = "y" boundTo true
    val yFalse = "y" boundTo false
    val distA = bernoulli("a", 1)
    val distB = bernoulli("b", 1)
    val aTrue = "a" boundTo true
    val aFalse = "a" boundTo false
    val bTrue = "b" boundTo true
    val bFalse = "b" boundTo false

    "marginalize correctly" in {
      val joint = distX * distY
      joint.f.isNormalized === true
      val xMarginalized = joint.marginalFor("x")
      xMarginalized.f.isNormalized === true
      xMarginalized(xTrue) must beApproximately(distX(xTrue))
      xMarginalized(xFalse) must beApproximately(distX(xFalse))
      val yMarginalized = joint.marginalFor("y")
      yMarginalized.f.isNormalized === true
      yMarginalized(yTrue) must beApproximately(distY(yTrue))
      yMarginalized(yFalse) must beApproximately(distY(yFalse))
    }

    "support negation correctly" in {
      val notA = !distA
      notA(aFalse) === 1
      notA(aTrue) === 0
    }

    "support disjunction correctly" in {
      val aOrB = distA || distB
      aOrB(Config(aTrue, bTrue)) === 1
      aOrB(Config(aTrue, bFalse)) === 1
      aOrB(Config(aFalse, bTrue)) === 1
      aOrB(Config(aFalse, bFalse)) === 0
    }

    "support conjunction correctly" in {
      val aAndB = distA && distB
      aAndB(Config(aTrue, bTrue)) === 1
      aAndB(Config(aTrue, bFalse)) === 0
      aAndB(Config(aFalse, bTrue)) === 0
      aAndB(Config(aFalse, bFalse)) === 0
    }

    "support implication correctly" in {
      val aThenB = distA -> distB
      aThenB(Config(aTrue, bTrue)) === 1
      aThenB(Config(aTrue, bFalse)) === 0
      aThenB(Config(aFalse, bTrue)) === 1
      aThenB(Config(aFalse, bFalse)) === 1
    }

    "support equivalence correctly" in {
      val aWhenB = distA <-> distB
      aWhenB(Config(aTrue, bTrue)) === 1
      aWhenB(Config(aTrue, bFalse)) === 0
      aWhenB(Config(aFalse, bTrue)) === 0
      aWhenB(Config(aFalse, bFalse)) === 1
    }

  }

  // TODO: Factor into TestHelper class.
  def beApproximately(x: Double) = beCloseTo(x, 0.000000001)

}
