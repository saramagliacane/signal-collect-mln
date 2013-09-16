package com.signalcollect.bp

import org.scalatest._

class FactorSpec extends FlatSpec with ShouldMatchers {

  val fA1 = Factor() + ("a", 0.4)
  val fA2 = Factor() + ("a", 0.1)
  val fB = Factor() + ("b", 0.1)

  "Factor" should "normalize correctly for one value" in {
    fA1.normalize("a") should be(1.0)
  }

  it should "normalize correctly for two values" in {
    val fAB = fA1 + fB
    fAB.normalize("a") should beApproximately(0.8)
  }

  it should "support addition" in {
    val fAadded = fA1 + fA2
    fAadded("a") should beApproximately(0.5)
  }

  it should "support subtraction" in {
    val fAsubtracted = fA1 - fA2
    fAsubtracted("a") should beApproximately(0.3)
  }

  it should "support multiplication" in {
    val fAmultiplied = fA1 * fA2
    fAmultiplied("a") should beApproximately(0.04)
  }

  it should "support division" in {
    val fAmultiplied = fA1 * fA2
    val shouldBeA1 = fAmultiplied / fA2
    val shouldBeA2 = fAmultiplied / fA1
    shouldBeA1("a") should beApproximately(fA1("a"))
    shouldBeA2("a") should beApproximately(fA2("a"))
  }

  //TODO: Add tests for logic stuff.

  def beApproximately(x: Double) = be(x +- 0.000000001)

}
