package com.signalcollect.mln

object SoftBool {
  implicit def doubleToSoftBool(d: Double) = SoftBool(d)
  def not(a: Double): Double = 1 - a
  def or(a: Double, b: Double): Double = math.min(1, a + b)
  def and(a: Double, b: Double): Double = math.max(0, a + b - 1)
  def implies(a: Double, b: Double): Double = or(not(a), b)
  def equivalent(a: Double, b: Double): Double =
    and((implies(a, b)), implies(b, a))
}

case class SoftBool(value: Double) extends AnyVal {
  import SoftBool._
  def unary_! = not(value)
  def ||(other: Double) = or(value, other)
  def &&(other: Double) = and(value, other)
  def ->(other: Double) = implies(value, other)
  def <->(other: Double) = equivalent(value, other)
}