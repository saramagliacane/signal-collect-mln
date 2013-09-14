package com.signalcollect.mln

/**
 * Represents a function from instances of Value to Double.
 * This function is stored inside the map @param map and the 
 * function returns 0 for parameters that are not stored in the map.
 * 
 * Factors are immutable and operations on them return new factors. 
 */
case class Factor[Value](
  val map: Map[Value, Double] = Map[Value, Double]())
    extends Function1[Value, Double] {

  def addValue(e: Value, probability: Double): Factor[Value] = {
    assert(probability >= 0)
    val newProbabilities = map + ((e, probability))
    Factor(newProbabilities)
  }

  def apply(e: Value): Double = {
    map.get(e).getOrElse(0)
  }

  def normalize: Factor[Value] = {
    if (!isNormalized) {
      val probabilitySum = map.values.sum
      val newProbabilities = {
        if (probabilitySum == 0) {
          val uniformEventProbability = 1.0 / map.size
          map map {
            case (event, _) =>
              (event, uniformEventProbability)
          }
        } else {
          map map {
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
    val newProbabilities = map flatMap {
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
        val p1 = map.get(value).getOrElse(0.0)
        val p2 = that.map.get(value).getOrElse(0.0)
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

  private lazy val sum = map.values.sum

  protected def intersection(that: Factor[Value]): Set[Value] =
    map.keySet.intersect(that.map.keySet)

  protected def union(that: Factor[Value]): Set[Value] =
    map.keySet.union(that.map.keySet)
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
