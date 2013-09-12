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

object Distribution {
  def bernoulli(name: String, probabilitySuccess: Double): Distribution = {
    val varTrue = Variable(name, true)
    val varFalse = Variable(name, false)
    val probabilityFailure = 1 - probabilitySuccess
    new Distribution(Map(
      Set(varTrue) -> probabilitySuccess,
      Set(varFalse) -> probabilityFailure))
  }
}

class Distribution(
  probabilities: Map[Set[BoundVariable], Double] = Map[Set[BoundVariable], Double]())
    extends Factor(probabilities) {

  type Config = Set[BoundVariable]

  override def addValue(e: Set[BoundVariable], probability: Double): Distribution = {
    assert(probability >= 0)
    val newProbabilities = probabilities + ((e, probability))
    new Distribution(newProbabilities)
  }

  /**
   * Creates a joint distribution under the assumption that @this and
   * @that are independent.
   */
  def join(that: Distribution): Distribution = {
    if (this.equals(JoinIdentity)) that
    else if (that.equals(JoinIdentity)) this
    else {
      val composite = for {
        config1 <- probabilities.keys
        config2 <- that.probabilities.keys
      } yield (
        config1.union(config2),
        probabilities(config1) * that.probabilities(config2))
      new Distribution(composite.toMap)
    }
  }

  /**
   * Returns the marginal distribution for @variable.
   */
  def marginalDistribution(variable: Variable): Distribution = {
    val groupedByBinding = probabilities.groupBy {
      case (config, probability) =>
        config.filter(_.name == variable.name).head
    }
    val marginalProbabilities = groupedByBinding map {
      case (binding, config) => (Set(binding), config.values.sum)
    }
    new Distribution(marginalProbabilities.toMap)
  }

}

object JoinIdentity extends Distribution