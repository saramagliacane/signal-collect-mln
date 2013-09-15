package com.signalcollect.bp

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

  def normalize = Distribution(f.normalize)
  def isNormalized = f.isNormalized

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

  /**
   * Delegate equals directly to map.
   */
  override def equals(other: Any): Boolean = {
    other match {
      case d: Distribution => f.map.equals(d.f.map)
      case other           => false
    }
  }

  /**
   * Delegate hashCode directly to map.
   */
  override def hashCode = f.map.hashCode

}

object Distribution {

  implicit def stringToVar(name: String): UnboundVariable = Variable(name)

  def bernoulli(
    name: String,
    probabilitySuccess: Double) = {
    val varTrue = name boundTo true
    val varFalse = name boundTo false
    val probabilityFailure = 1 - probabilitySuccess
    Distribution(Factor(Map(
      Config(varTrue) -> probabilitySuccess,
      Config(varFalse) -> probabilityFailure)))
  }

}

object JoinIdentity extends Distribution
