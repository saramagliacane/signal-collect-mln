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
    forValues(intersection(that), that, _ * _)
  }

  def /(that: Factor[Value]): Factor[Value] = {
    forValues(intersection(that), that, _ / _)
  }

  def +(that: Factor[Value]): Factor[Value] = {
    forValues(union(that), that, _ + _)
  }

  def -(that: Factor[Value]): Factor[Value] = {
    forValues(union(that), that, _ - _)
  }

  /**
   * Correspond to boolean logic operations when true is represented
   * as 1 and false as 0.
   */
  def ||(that: Factor[Value]): Factor[Value] =
    forValues(union(that), that, SoftBool.||)
  def &&(that: Factor[Value]): Factor[Value] =
    forValues(union(that), that, SoftBool.&&)
  def ->(that: Factor[Value]): Factor[Value] =
    forValues(union(that), that, SoftBool.->)
  def <->(that: Factor[Value]): Factor[Value] =
    forValues(union(that), that, SoftBool.<->)
  def unary_!(): Factor[Value] = {
    val newProbabilities = probabilities flatMap {
      case (value, probability) =>
        val newP = 1 - probability
        if (newP > 0) {
          Some((value, newP))
        } else {
          None
        }
    }
    Factor(newProbabilities.toMap)
  }

  private def forValues(
    values: Set[Value],
    that: Factor[Value],
    op: (Double, Double) => Double): Factor[Value] = {
    val newProbabilities = values flatMap {
      value =>
        val p1 = probabilities.get(value).getOrElse(0.0)
        val p2 = that.probabilities.get(value).getOrElse(0.0)
        val newP = op(p1, p2)
        // Only store positive probabilities. 
        if (newP > 0) {
          Some((value, newP))
        } else {
          None
        }
    }
    Factor(newProbabilities.toMap)
  }

  def isNormalized = math.abs(sum - 1.0) < 0.000001

  private lazy val sum = probabilities.values.sum

  protected def intersection(that: Factor[Value]): Set[Value] =
    probabilities.keySet.intersect(that.probabilities.keySet)

  protected def union(that: Factor[Value]): Set[Value] =
    probabilities.keySet.union(that.probabilities.keySet)
}

object SoftBool {
  def not(a: Double) = 1 - a
  def ||(a: Double, b: Double) = math.min(1, a + b)
  def &&(a: Double, b: Double) = math.max(0, a + b - 1)
  def ->(a: Double, b: Double) = ||(not(a), b)
  def <->(a: Double, b: Double) = &&((->(a, b)), ->(a, b))
}

case class SoftBool(value: Double) extends AnyVal {
  def unary_! = SoftBool(SoftBool.not(value))
  def ||(other: SoftBool) = SoftBool(SoftBool.||(value, other.value))
  def &&(other: SoftBool) = SoftBool(SoftBool.&&(value, other.value))
  def ->(other: SoftBool) = SoftBool(SoftBool.->(value, other.value))
  def <->(other: SoftBool) = SoftBool(SoftBool.<->(value, other.value))
}
