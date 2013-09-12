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

  protected override def newInstance(p: Map[Config, Double]): this.type =
    (new Distribution(p)).asInstanceOf[this.type]

  def *(that: Distribution): Distribution = {
    forValues(intersection(that), that, _ * _, MultiplicativeIdentity.forType[Distribution])
  }

  def /(that: Distribution): Distribution = {
    forValues(intersection(that), that, _ / _, MultiplicativeIdentity.forType[Distribution])
  }

  def +(that: Distribution): Distribution = {
    forValues(intersection(that), that, _ + _, AdditiveIdentity.forType[Distribution])
  }

  def -(that: Distribution): Distribution = {
    forValues(intersection(that), that, _ - _, AdditiveIdentity.forType[Distribution])
  }

  private def forValues(
    values: Set[Config],
    that: Distribution,
    op: (Double, Double) => Double,
    neutral: Distribution): Distribution = {
    if (this.equals(neutral)) that
    else if (that.equals(neutral)) this
    else {
      val newEventProbabilities = values map {
        event =>
          val p1 = probabilities(event)
          val p2 = that.probabilities(event)
          (event, op(p1, p2))
      }
      newInstance(newEventProbabilities.toMap)
    }
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

object JoinIdentity extends Factor {
  def forType[T <: Factor[_]] = JoinIdentity.asInstanceOf[T]
}

object MultiplicativeIdentity extends Factor(Map().withDefaultValue(1)) {
  def forType[T <: Factor[_]] = MultiplicativeIdentity.asInstanceOf[T]
}

object AdditiveIdentity extends Factor(Map().withDefaultValue(0)) {
  def forType[T <: Factor[_]] = AdditiveIdentity.asInstanceOf[T]
}
