package com.signalcollect.bp

/**
 * Represents a function from configurations to their potentially
 * unnormalized probabilities. The function is stored as the factor
 * @param f.
 */
case class Distribution(
  f: Factor[Config] = Factor[Config]())
    extends Function1[Config, Double] {

  implicit def eps = 0.000001

  /**
   * Returns the probability of this configuration.
   */
  def apply(c: Config) = f.map.get(c).getOrElse(0)

  def isDefinedAt(input: Config) = true

  def configs: Set[Config] = f.validInputs

  def unary_! = Distribution(!f)
  def +(that: Distribution) = join(that)(Ops.+)
  def -(that: Distribution) = join(that)(Ops.-)
  def *(that: Distribution) = join(that)(Ops.*)
  def /(that: Distribution) = join(that)(Ops./)
  def ||(that: Distribution) = join(that)(Ops.or)
  def &&(that: Distribution) = join(that)(Ops.and)
  def ->(that: Distribution) = join(that)(Ops.implies)
  def <->(that: Distribution) = join(that)(Ops.equivalent)

  def isNormalized(implicit eps: Double) = math.abs(f.sum - 1.0) < eps

  def normalize(implicit eps: Double): Distribution = {
    val purged = if (f.isAllOutputPositive) {
      this
    } else {
      Distribution(f.purge(eps))
    }
    if (purged.isNormalized(eps)) {
      purged
    } else {
      val sum = purged.f.sum
      val newMappings = purged.f.map map {
        case (value, unnormalizedOutput) =>
          val newMapping = unnormalizedOutput / sum
          (value, newMapping)
      }
      Distribution(Factor(newMappings.toMap))
    }
  }

  /**
   * Creates a joint distribution and merges probabilities with the
   * function @param op.
   */
  protected def join(
    that: Distribution)(
      op: (Option[Double], Option[Double]) => Option[Double]): Distribution = {
    if (this.equals(JoinIdentity)) that
    else if (that.equals(JoinIdentity)) this
    else {
      val composite = for {
        config1 <- configs
        config2 <- that.configs
        composedOutput <- op(Some(this(config1)), Some(that(config2)))
      } yield (
        config1.combine(config2),
        composedOutput)
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

  override def toString = s"Distribution(${f.toString})"

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
