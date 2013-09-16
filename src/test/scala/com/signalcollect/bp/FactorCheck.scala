package com.signalcollect.bp

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

object FactorCheck extends Properties("Factor") {

  implicit def arbitrayFactor = Arbitrary(
    Arbitrary.arbitrary[Map[Int, Double]] map (Factor(_).normalize) suchThat (f => f.map.values.forall(_ > 0)))

  property("+") = forAll((f1: Factor[Int], f2: Factor[Int]) =>
    f1 + f2 == f2 + f1)

  property("+ and - are inverse") = forAll((f1: Factor[Int], f2: Factor[Int]) =>
    (f1 + f2 - f2) approximatelyEquals f1)

  property("*") = forAll((f1: Factor[Int], f2: Factor[Int]) =>
    f1 * f2 == f2 * f1)

  property("* and / are inverse") = forAll((f1: Factor[Int], f2: Factor[Int]) =>
    if (f1.map.keySet == f2.map.keySet) {
      ((f1 * f2) / f2) approximatelyEquals f1
    } else {
      true
    })
}