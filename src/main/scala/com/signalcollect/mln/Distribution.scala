package com.signalcollect.mln

object Variable {
  def apply(name: String) = new UnboundVariable(name)
  def apply(name: String, value: Any) = new BoundVariable(name, value)
}

trait Variable {
  def name: String
  def bind(value: Any) = new BoundVariable(name, value)
  def unbound = new UnboundVariable(name)
}

case class UnboundVariable(name: String) extends Variable

case class BoundVariable(name: String, value: Any) extends Variable

case class Config(boundVars: Set[BoundVariable]) {
  def combine(other: Config) = Config(boundVars.union(other.boundVars))
  /**
   * Requires for the variable to be part of this configuration.
   */
  def getVariable(name: String): BoundVariable =
    boundVars.filter(_.name == name).head
}

object Distribution {

  def bernoulli(name: String, probabilitySuccess: Double): Distribution = {
    val varTrue = Variable(name, true)
    val varFalse = Variable(name, false)
    val probabilityFailure = 1 - probabilitySuccess
    new Distribution(Factor(Map(
      Config(Set(varTrue)) -> probabilitySuccess,
      Config(Set(varFalse)) -> probabilityFailure)))
  }

}

case class Distribution(
    val f: Factor[Config] = Factor[Config]()) {

  def *(that: Distribution) = Distribution(f * that.f)
  def /(that: Distribution) = Distribution(f / that.f)
  def +(that: Distribution) = Distribution(f + that.f)
  def -(that: Distribution) = Distribution(f - that.f)

  /**
   * Creates a joint distribution under the assumption that @this and
   * @that are independent.
   */
  def join(that: Distribution): Distribution = {
    if (this.equals(JoinIdentity)) that
    else if (that.equals(JoinIdentity)) this
    else {
      val composite = for {
        config1 <- f.probabilities.keys
        config2 <- that.f.probabilities.keys
      } yield (
        config1.combine(config2),
        f.probabilities(config1) * that.f.probabilities(config2))
      Distribution(Factor(composite.toMap))
    }
  }

  /**
   * Returns the marginal distribution for @variable.
   */
  def marginalDistribution(variable: Variable): Distribution = {
    val groupedByBinding = f.probabilities.groupBy {
      case (config, probability) => config.getVariable(variable.name)
    }
    val marginalProbabilities = groupedByBinding map {
      case (binding, config) => (Config(Set(binding)), config.values.sum)
    }
    Distribution(Factor(marginalProbabilities.toMap))
  }

}
