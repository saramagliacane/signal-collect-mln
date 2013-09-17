package com.signalcollect.bp

object Ops {
  def +(optA: Option[Double], optB: Option[Double]): Option[Double] = {
    (optA, optB) match {
      case (Some(a), Some(b)) => Some(a + b)
      case (Some(a), None) => Some(a)
      case (None, Some(b)) => Some(b)
      case (None, None) => None
    }
  }
  def -(optA: Option[Double], optB: Option[Double]): Option[Double] = {
    (optA, optB) match {
      case (Some(a), Some(b)) => Some(a - b)
      case (Some(a), None) => Some(a)
      case (None, Some(b)) => Some(-b)
      case (None, None) => None
    }
  }
  def *(optA: Option[Double], optB: Option[Double]): Option[Double] = {
    (optA, optB) match {
      case (Some(a), Some(b)) => Some(a * b)
      case (Some(a), None) => Some(0)
      case (None, Some(b)) => Some(0)
      case (None, None) => None
    }
  }
  def /(optA: Option[Double], optB: Option[Double]): Option[Double] = {
    (optA, optB) match {
      case (Some(a), Some(b)) => if (b != 0) Some(a / b) else None
      case (Some(a), None) => Some(0)
      case (None, Some(b)) => Some(0)
      case (None, None) => None
    }
  }
  def not(a: Option[Double]): Option[Double] = a map (1 - _)
  def or(optA: Option[Double], optB: Option[Double]): Option[Double] = {
    (optA, optB) match {
      case (Some(a), Some(b)) => Some(math.min(1, a + b))
      case (Some(a), None) => Some(a)
      case (None, Some(b)) => Some(b)
      case (None, None) => None
    }
  }
  def and(optA: Option[Double], optB: Option[Double]): Option[Double] = {
    (optA, optB) match {
      case (Some(a), Some(b)) => Some(math.max(0, a + b - 1))
      case (Some(a), None) => None
      case (None, Some(b)) => None
      case (None, None) => None
    }
  }
  def implies(a: Option[Double], b: Option[Double]): Option[Double] = or(not(a), b)
  def equivalent(a: Option[Double], b: Option[Double]): Option[Double] =
    and((implies(a, b)), implies(b, a))
}