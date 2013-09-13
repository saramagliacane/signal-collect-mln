package com.signalcollect.mln

case class Factor[Value](
  val probabilities: Map[Value, Double] = Map[Value, Double]())
    extends Function1[Value, Double] {

  def addValue(e: Value, probability: Double): Factor[Value] = {
    assert(probability >= 0)
    val newProbabilities = probabilities + ((e, probability))
    Factor(newProbabilities)
  }

  def apply(e: Value): Double = {
    probabilities.get(e).getOrElse(0)
  }

  def normalize: Factor[Value] = {
    if (!isNormalized) {
      val probabilitySum = probabilities.values.sum
      val newProbabilities = {
        if (probabilitySum == 0) {
          val uniformEventProbability = 1.0 / probabilities.size
          probabilities map {
            case (event, _) =>
              (event, uniformEventProbability)
          }
        } else {
          probabilities map {
            case (event, unnormalizedProbability) =>
              (event, unnormalizedProbability / probabilitySum)
          }
        }
      }
      Factor(newProbabilities)
    } else {
      this
    }
  }

  def *(that: Factor[Value]): Factor[Value] = {
    forValues(intersection(that), that, _ * _, MultiplicativeIdentity.forType[Factor[Value]])
  }

  def /(that: Factor[Value]): Factor[Value] = {
    forValues(intersection(that), that, _ / _, MultiplicativeIdentity.forType[Factor[Value]])
  }

  def +(that: Factor[Value]): Factor[Value] = {
    forValues(union(that), that, _ + _, AdditiveIdentity.forType[Factor[Value]])
  }

  def -(that: Factor[Value]): Factor[Value] = {
    forValues(union(that), that, _ - _, AdditiveIdentity.forType[Factor[Value]])
  }

  private def forValues(
    values: Set[Value],
    that: Factor[Value],
    op: (Double, Double) => Double,
    neutral: Factor[Value]): Factor[Value] = {
    if (this.equals(neutral)) that
    else if (that.equals(neutral)) this
    else {
      val newProbabilities = values map {
        value =>
          val p1 = probabilities.get(value).getOrElse(0.0)
          val p2 = that.probabilities.get(value).getOrElse(0.0)
          val newP = op(p1, p2)
          assert(newP >= 0)
          (value, newP)
      }
      Factor(newProbabilities.toMap)
    }
  }

  def isNormalized = math.abs(sum - 1.0) < 0.000001

  private lazy val sum = probabilities.values.sum

  protected def intersection(that: Factor[Value]): Set[Value] =
    probabilities.keySet.intersect(that.probabilities.keySet)

  protected def union(that: Factor[Value]): Set[Value] =
    probabilities.keySet.union(that.probabilities.keySet)

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
