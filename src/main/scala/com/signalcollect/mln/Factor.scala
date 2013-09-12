package com.signalcollect.mln

class Factor[Value](
  val probabilities: Map[Value, Double] = Map[Value, Double]())
    extends Function1[Value, Double] {

  protected def newInstance(p: Map[Value, Double]): this.type =
    (new Factor(p)).asInstanceOf[this.type]

  def addValue(e: Value, probability: Double): this.type = {
    assert(probability >= 0)
    val newProbabilities = probabilities + ((e, probability))
    newInstance(newProbabilities)
  }

  def apply(e: Value): Double = {
    probabilities.get(e).getOrElse(0)
  }

  def normalize: this.type = {
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
      newInstance(newProbabilities)
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
    forValues(intersection(that), that, _ + _, AdditiveIdentity.forType[Factor[Value]])
  }

  def -(that: Factor[Value]): Factor[Value] = {
    forValues(intersection(that), that, _ - _, AdditiveIdentity.forType[Factor[Value]])
  }

  private def forValues(
    values: Set[Value],
    that: Factor[Value],
    op: (Double, Double) => Double,
    neutral: Factor[Value]): Factor[Value] = {
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

  def isNormalized = math.abs(sum - 1.0) < 0.000001

  private lazy val sum = probabilities.values.sum

  protected def intersection(that: Factor[Value]): Set[Value] =
    probabilities.keySet.intersect(that.probabilities.keySet)

  protected def union(that: Factor[Value]): Set[Value] =
    probabilities.keySet.union(that.probabilities.keySet)

  override def toString = {
    val sb = new StringBuilder
    for (t <- probabilities) {
      t match {
        case (event, probability) =>
          sb.append(s"Value $event: $probability\n")
      }
    }
    sb.toString
  }

  override def equals(other: Any): Boolean = {
    other match {
      case d: Distribution => probabilities.equals(d.probabilities)
      case other           => false
    }
  }

}