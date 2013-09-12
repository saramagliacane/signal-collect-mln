package com.signalcollect.mln

import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner
import Distribution._

@RunWith(classOf[JUnitRunner])
class DistributionSpec extends SpecificationWithJUnit with Serializable {

  sequential

  "Distribution" should {
    "marginalize correctly" in {
      val distA = bernoulli("a", 0.6)
      val distB = bernoulli("b", 0.9)
      val joint = distA.join(distB)
      val aMarginalized = joint.marginalDistribution(Variable("a"))
      aMarginalized === distA
    }

  }
}