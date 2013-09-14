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

  implicit def stringToVar(name: String): UnboundVariable = Variable(name)
  
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

  def *(that: Distribution) = join(that)(_ * _)
  def /(that: Distribution) = join(that)(_ / _)
  def +(that: Distribution) = join(that)(_ + _)  
  def -(that: Distribution) = join(that)(_ - _)
  def ||(that: Distribution) = join(that)(SoftBool.||)
  def &&(that: Distribution) = join(that)(SoftBool.&&)
  def ->(that: Distribution) = join(that)(SoftBool.->)
  def <->(that: Distribution) = join(that)(SoftBool.<->)

  /**
   * Creates a joint distribution and merges probabilities with the
   * function @param op.
   */
  protected def join(that: Distribution)(op: (Double, Double) => Double): Distribution = {
    if (this.equals(JoinIdentity)) that
    else if (that.equals(JoinIdentity)) this
    else {
      val composite = for {
        config1 <- f.probabilities.keys
        config2 <- that.f.probabilities.keys
      } yield (
        config1.combine(config2),
        op(f.probabilities(config1), that.f.probabilities(config2)))
      Distribution(Factor(composite.toMap))
    }
  }

  /**
   * Returns the marginal distribution for @param variable.
   */
  def marginalFor(variable: Variable): Distribution = {
    val groupedByBinding = f.probabilities.groupBy {
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