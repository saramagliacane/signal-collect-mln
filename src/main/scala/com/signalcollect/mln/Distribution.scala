package com.signalcollect.mln

object Distribution {

  implicit def stringToVar(name: String): UnboundVariable = Variable(name)

  def bernoulli(
    name: String,
    probabilitySuccess: Double): Distribution = {
    val varTrue = Variable(name, true)
    val varFalse = Variable(name, false)
    val probabilityFailure = 1 - probabilitySuccess
    new Distribution(Factor(Map(
      Config(Set(varTrue)) -> probabilitySuccess,
      Config(Set(varFalse)) -> probabilityFailure)))
  }

}

/**
 * Represents a function from configurations to their potentially
 * unnormalized probabilities. The function is stored as the factor
 * @param f.
 */
case class Distribution(
    f: Factor[Config] = Factor[Config]()) {

  /**
   * Returns the probability of this configuration.
   */
  def apply(c: Config) = f(c)

  def unary_! = Distribution(!f)
  def *(that: Distribution) = join(that)(_ * _)
  def /(that: Distribution) = join(that)(_ / _)
  def +(that: Distribution) = join(that)(_ + _)
  def -(that: Distribution) = join(that)(_ - _)
  def ||(that: Distribution) = join(that)(SoftBool.or)
  def &&(that: Distribution) = join(that)(SoftBool.and)
  def ->(that: Distribution) = join(that)(SoftBool.implies)
  def <->(that: Distribution) = join(that)(SoftBool.equivalent)

  /**
   * Creates a joint distribution and merges probabilities with the
   * function @param op.
   */
  protected def join(
    that: Distribution)(
      op: (Double, Double) => Double): Distribution = {
    if (this.equals(JoinIdentity)) that
    else if (that.equals(JoinIdentity)) this
    else {
      val composite = for {
        config1 <- f.map.keys
        config2 <- that.f.map.keys
      } yield (
        config1.combine(config2),
        op(f(config1), that.f(config2)))
      Distribution(Factor(composite.toMap))
    }
  }

  /**
   * Returns the marginal distribution for @param variable.
   */
  def marginalFor(variable: Variable): Distribution = {
    val groupedByBinding = f.map.groupBy {
      case (config, probability) => config.getVariable(variable.name)
    }
    val marginalProbabilities = groupedByBinding map {
      case (binding, config) => (Config(Set(binding)), config.values.sum)
    }
    Distribution(Factor(marginalProbabilities.toMap))
  }

}

object JoinIdentity extends Factor {
  def forType[T <: Factor[_]] = JoinIdentity.asInstanceOf[T]
}
