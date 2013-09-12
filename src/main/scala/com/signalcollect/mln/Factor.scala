package com.signalcollect.mln

import scala.util.Random
import scala.collection.immutable.Stream.consWrapper

class Factor[Value](
  val probabilities: Map[Value, Double] = Map[Value, Double]())
    extends Function1[Value, Double] {

  def addValue(e: Value, probability: Double): Factor[Value] = {
    assert(probability >= 0)
    val newProbabilities = probabilities + ((e, probability))
    new Factor(newProbabilities)
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
      new Factor(newProbabilities)
    } else {
      this
    }
  }

  def isNormalized = math.abs(sum - 1.0) < 0.000001

  private lazy val sum = probabilities.values.sum

  def *(that: Factor[Value]): Factor[Value] = {
    forEventsInFactors(intersection(that), that, _ * _, MultiplicativeIdentity.asInstanceOf[Factor[Value]])
  }

  def /(that: Factor[Value]): Factor[Value] = {
    forEventsInFactors(intersection(that), that, _ / _, MultiplicativeIdentity.asInstanceOf[Factor[Value]])
  }

  def +(that: Factor[Value]): Factor[Value] = {
    forEventsInFactors(intersection(that), that, _ + _, AdditiveIdentity.asInstanceOf[Factor[Value]])
  }

  def -(that: Factor[Value]): Factor[Value] = {
    forEventsInFactors(intersection(that), that, _ - _, AdditiveIdentity.asInstanceOf[Factor[Value]])
  }

  private def forEventsInFactors(events: Set[Value], that: Factor[Value], op: (Double, Double) => Double, neutral: Factor[Value]): Factor[Value] = {
    if (Factor.this.equals(neutral)) that
    else if (that.equals(neutral)) this
    else {
      val newEventProbabilities = events map {
        event =>
          val p1 = probabilities(event)
          val p2 = that.probabilities(event)
          (event, op(p1, p2))
      }
      new Factor(newEventProbabilities.toMap)
    }
  }

  private def intersection(that: Factor[Value]): Set[Value] = probabilities.keySet.intersect(that.probabilities.keySet)

  private def union(that: Factor[Value]): Set[Value] = probabilities.keySet.union(that.probabilities.keySet)

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

object MultiplicativeIdentity extends Factor[Any](Map().withDefaultValue(1))

object AdditiveIdentity extends Factor[Any](Map().withDefaultValue(0))