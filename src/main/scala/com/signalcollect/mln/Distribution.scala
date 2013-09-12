package com.signalcollect.mln

class Distribution[Value](
    probabilities: Map[Set[Set[Value]], Double] = Map[Set[Set[Value]], Double]()
    ) extends Factor(probabilities) {

  def join(that: Distribution[Value]): Distribution[Value] = {
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
  
  def marginalize(variable: Set[Value]): Distribution[Value] = ???
  
}

object JoinIdentity extends Factor[Any]