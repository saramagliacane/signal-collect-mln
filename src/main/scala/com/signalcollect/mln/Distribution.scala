package com.signalcollect.mln

class Variable(val name: String, val domain: Set[Any])

trait Binding {
  def binding: Any
}

class Distribution(
  probabilities: Map[Set[Variable with Binding], Double] = Map[Set[Variable with Binding], Double]()) extends Factor(probabilities) {

  type Config = Set[Variable with Binding]

  def join(that: Distribution): Distribution = {
    if (this.equals(JoinIdentity)) that
    else if (that.equals(JoinIdentity)) this
    else {
      val composite = for {
        variableDistribution1 <- probabilities.keys
        variableDistribution2 <- that.probabilities.keys
      } yield (
        variableDistribution1.union(variableDistribution2),
        probabilities(variableDistribution1) * that.probabilities(variableDistribution2))
      new Distribution(composite.toMap)
    }
  }

  /**
   * Returns the marginal distribution for the passed variable.
   */
  def marginalDistribution(variable: Variable): Distribution = {
    val groupedByBinding: Map[Variable with Binding, Map[Set[Variable with Binding], Double]] = probabilities.groupBy {
      case (config, probability) => config.filter(_.name == variable.name).head
    }
    val marginalProbabilities = for {
      binding <- groupedByBinding
    } yield (Set(binding._1), binding._2.values.sum)
    new Distribution(marginalProbabilities.toMap)
  }

}

object JoinIdentity extends Distribution