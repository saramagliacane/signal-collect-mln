package com.signalcollect.bp

/**
 * Represents a function from instances of Value to Doubles > 0.
 * This function is stored inside the map @param map and the
 * function returns 0 for parameters that are not stored in the map.
 *
 * Factors are immutable and operations on them return new factors.
 */
case class Factor[Value](
  map: Map[Value, Double] = Map[Value, Double]())
  extends Function1[Value, Double] {
  
  /**
   * Creates a new factor that returns @param result for parameter
   * @param parameter.
   */
  def +(parameter: Value, result: Double): Factor[Value] = {
    assert(result > eps)
    Factor(map + ((parameter, result)))
  }

  def apply(e: Value): Double = map.get(e).getOrElse(0)

  def normalize: Factor[Value] = {
    if (!isNormalized) {
      val probabilitySum = map.values.sum
      val newMappings = {
        if (probabilitySum == 0) {
          val uniformEventProbability = 1.0 / map.size
          map map {
            case (event, _) =>
              (event, uniformEventProbability)
          }
        } else {
          map flatMap {
            case (value, unnormalizedMapping) =>
              val newMapping = unnormalizedMapping / probabilitySum
              if (newMapping > eps) {
                Some(value, newMapping)
              } else {
                None
              }
          }
        }
      }
      Factor(newMappings)
    } else {
      this
    }
  }

  /**
   * These operations correspond to combining the results for each factor
   * with the respective operation.
   * @example: f1(a) = 0.5
   *           f2(a) = 0.2
   *           f3 = f1 * f2
   *           f3(a) = 0.5 * 0.2 = 0.1
   */
  def *(that: Factor[Value]) = forValues(intersection(that), that, _ * _)
  def /(that: Factor[Value]) = forValues(intersection(that), that, _ / _)
  def +(that: Factor[Value]) = forValues(union(that), that, _ + _)
  def -(that: Factor[Value]) = forValues(union(that), that, _ - _)

  /**
   * These operations correspond to boolean logic operations when true is
   * represented as 1 and false as 0.
   */
  def ||(that: Factor[Value]): Factor[Value] =
    forValues(union(that), that, SoftBool.or)
  def &&(that: Factor[Value]): Factor[Value] =
    forValues(union(that), that, SoftBool.and)
  def ->(that: Factor[Value]): Factor[Value] =
    forValues(union(that), that, SoftBool.implies)
  def <->(that: Factor[Value]): Factor[Value] =
    forValues(union(that), that, SoftBool.equivalent)
  def unary_!(): Factor[Value] = {
    val newMappings = map flatMap {
      case (value, probability) =>
        val newP = 1 - probability
        // Only store probabilities > 0.
        if (newP > eps) {
          Some((value, newP))
        } else {
          None
        }
    }
    Factor(newMappings.toMap)
  }

  private def forValues(
    values: Set[Value],
    that: Factor[Value],
    op: (Double, Double) => Double): Factor[Value] = {
    val newMappings = values flatMap {
      value =>
        val newP = op(this(value), that(value))
        // Only store probabilities > 0.
        if (newP > eps) {
          Some((value, newP))
        } else {
          None
        }
    }
    Factor(newMappings.toMap)
  }

  private def eps = 0.000001

  def approximatelyEquals(other: Factor[Value]) = {
    def approximatelyEqual(a: Double, b: Double) = {
      math.abs(a - b) < eps
    }
    val all = intersection(other)
    all.size == map.size && all.size == other.map.size && all.forall {
      k => approximatelyEqual(other(k), this(k))
    }

  }

  def isNormalized = math.abs(sum - 1.0) < eps

  private lazy val sum = map.values.sum

  protected def intersection(that: Factor[Value]): Set[Value] =
    map.keySet.intersect(that.map.keySet)

  protected def union(that: Factor[Value]): Set[Value] =
    map.keySet.union(that.map.keySet)

  override def toString = map.mkString("\n\t", "\n\t", "\n")
}
