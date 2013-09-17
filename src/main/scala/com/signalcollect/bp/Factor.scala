package com.signalcollect.bp

/**
 * Represents a partial function from instances of Domain to Doubles.
 * This function is stored inside the map @param map.
 *
 * Factors are immutable and operations on them return new factors.
 */
case class Factor[Domain](
  val map: Map[Domain, Double] = Map[Domain, Double]())
    extends PartialFunction[Domain, Double] {

  def purge(a: String)(implicit eps: Double) = Factor(map.filter(_._2 < eps))

  def validInputs: Set[Domain] = map.keySet

  def isDefinedAt(input: Domain) = map.keySet.contains(input)

  /**
   * Creates a new factor that returns @param result for parameter
   * @param parameter.
   */
  def +(input: Domain, output: Double): Factor[Domain] = {
    Factor(map + ((input, output)))
  }

  def apply(input: Domain): Double = map(input)

  def get(input: Domain): Option[Double] = map.get(input)

  /**
   * These operations correspond to combining the results for each factor
   * with the respective operation.
   * @example: f1(a) = 0.5
   *           f2(a) = 0.2
   *           f3 = f1 * f2
   *           f3(a) = 0.5 * 0.2 = 0.1
   */
  def *(that: Factor[Domain]) = forInputs(intersection(that), that, Ops.*)
  def *(that: PartialFunction[Domain, Double]) = forInputs(map.keySet, that, Ops.*)

  def /(that: Factor[Domain]) = forInputs(intersection(that), that, Ops./)
  def /(that: PartialFunction[Domain, Double]) = forInputs(map.keySet, that, Ops./)

  def +(that: Factor[Domain]) = forInputs(union(that), that, Ops.+)
  def +(that: PartialFunction[Domain, Double]) = forInputs(map.keySet, that, Ops.+)

  def -(that: Factor[Domain]) = forInputs(union(that), that, Ops.-)
  def -(that: PartialFunction[Domain, Double]) = forInputs(map.keySet, that, Ops.-)

  /**
   * These operations correspond to boolean logic operations when true is
   * represented as 1 and false as 0.
   */
  def ||(that: Factor[Domain]) = forInputs(union(that), that, Ops.or)
  def ||(that: PartialFunction[Domain, Double]) = forInputs(map.keySet, that, Ops.or)
  def &&(that: Factor[Domain]) = forInputs(union(that), that, Ops.and)
  def &&(that: PartialFunction[Domain, Double]) = forInputs(map.keySet, that, Ops.and)
  def ->(that: Factor[Domain]) = forInputs(union(that), that, Ops.implies)
  def ->(that: PartialFunction[Domain, Double]) = forInputs(map.keySet, that, Ops.implies)
  def <->(that: Factor[Domain]) = forInputs(union(that), that, Ops.equivalent)
  def <->(that: PartialFunction[Domain, Double]) = forInputs(map.keySet, that, Ops.equivalent)
  def unary_!() = {
    val newMappings = map map {
      case (input, output) =>
        val newOutput = 1 - output
        (input, newOutput)
    }
    Factor(newMappings.toMap)
  }

  private def forInputs(
    inputs: Set[Domain],
    that: PartialFunction[Domain, Double],
    op: (Option[Double], Option[Double]) => Option[Double]): Factor[Domain] = {
    val newMappings = inputs flatMap {
      input =>
        val thisOutput = map.get(input)
        val thatOutput = if (that.isDefinedAt(input)) Some(that(input)) else None
        val newOutput = op(thisOutput, thatOutput)
        newOutput map ((input, _))
    }
    Factor(newMappings.toMap)
  }

  def approximatelyEquals(other: Factor[Domain], eps: Double = 0.000001) = {
      def approximatelyEqual(a: Double, b: Double) = {
        math.abs(a - b) < eps
      }
    val all = intersection(other)
    all.size == map.size && all.size == other.map.size && all.forall {
      k => approximatelyEqual(other(k), this(k))
    }

  }

  lazy val sum = map.values.sum
  lazy val isAllOutputPositive = map.values.forall(_ >= 0)

  protected def intersection(that: Factor[Domain]): Set[Domain] =
    map.keySet.intersect(that.map.keySet)

  protected def union(that: Factor[Domain]): Set[Domain] =
    map.keySet.union(that.map.keySet)

  override def toString = map.mkString("\n\t", "\n\t", "\n")
}

object Factor {
  implicit val eps: Double = 0.000001
}
