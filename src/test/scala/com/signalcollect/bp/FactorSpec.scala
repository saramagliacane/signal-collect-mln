package com.signalcollect.bp

import org.scalatest._
import org.scalacheck.Arbitrary
import org.scalatest.prop.Checkers
import org.scalacheck.Gen

class FactorSpec extends FlatSpec with ShouldMatchers with Checkers {

  val fA1 = Factor() + ("a", 0.4)
  val fA2 = Factor() + ("a", 0.1)
  val fB = Factor() + ("b", 0.1)

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

  val smallInteger = Gen.choose(0, 5)
  val smallDouble = Gen.choose(0, 1.0)
  implicit def smallMap = Arbitrary(Arbitrary.arbImmutableMap(Arbitrary(smallInteger), Arbitrary(smallDouble)).arbitrary map (Factor(_)))

  it should "support commutative addition" in {
    check(
      (f1: Factor[Int], f2: Factor[Int]) =>
        f1 + f2 == f2 + f1,
      minSuccessful(200))
  }

  it should "have a subtraction that is the inverse of addition " +
    "(for small numbers)" in {
      check(
        (f1: Factor[Int], f2: Factor[Int]) =>
          (f1 + f2 - f2).purge("a") approximatelyEquals f1.purge(),
        minSuccessful(200))
    }

  it should "support commutative multiplication" in {
    check(
      (f1: Factor[Int], f2: Factor[Int]) =>
        f1 * f2 == f2 * f1,
      minSuccessful(200))
  }

  it should "have a division that is the inverse of multiplication " +
    "(when both factors share all values)" in {
      check(
        (f1: Factor[Int], f2: Factor[Int]) =>
          if (f1.map.keySet == f2.map.keySet) {
            ((f1 * f2) / f2) approximatelyEquals f1
          } else {
            true
          },
        minSuccessful(200))
    }

  //TODO: Add tests for logic stuff.

  def beApproximately(x: Double) = be(x +- 0.000000001)

}
