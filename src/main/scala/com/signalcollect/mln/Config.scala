package com.signalcollect.mln

/**
 * A configuration consists of a set of bound variables and
 * represents a possible outcome in the context of a probability
 * distribution.
 */
case class Config(boundVars: Set[BoundVariable]) {
  def combine(other: Config) = Config(boundVars.union(other.boundVars))
  /**
   * Requires for the variable to be part of this configuration.
   */
  def getVariable(name: String): BoundVariable =
    boundVars.filter(_.name == name).head

  override def toString = boundVars.mkString(", ")
}

object Config {
  implicit def varToConfig(v: BoundVariable) = Config(Set(v))
  def apply(vs: BoundVariable*): Config = Config(vs.toSet)
}
