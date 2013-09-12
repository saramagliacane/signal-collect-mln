package com.signalcollect.mln

import scala.util.Random
import scala.collection.immutable.Stream.consWrapper

case class Event(impl: Set[Any]) {
  def union(that: Event): Event = Event(impl.union(that.impl))
}

object Event {
  implicit def set2Event(set: Set[Any]): Event = Event(set)
}

case class Factor(
  eventProbabilities: Map[Event, Double] = Map[Event, Double]())
  extends Function1[Event, Double] {

  def addEvent(e: Event, probability: Double): Factor = {
    assert(probability >= 0)
    val newProbabilities = eventProbabilities + ((e, probability))
    Factor(newProbabilities)
  }

  def apply(e: Event): Double = {
    eventProbabilities.get(e).getOrElse(0)
  }

  def normalize: Factor = {
    if (!isNormalized) {
      val probabilitySum = eventProbabilities.values.sum
      val newProbabilities = {
        if (probabilitySum == 0) {
          val uniformEventProbability = 1.0 / eventProbabilities.size
          eventProbabilities map {
            case (event, _) =>
              (event, uniformEventProbability)
          }
        } else {
          eventProbabilities map {
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

  def isNormalized = math.abs(sum - 1.0) < 0.000001

  private lazy val sum = eventProbabilities.values.sum

  override def toString = {
    val sb = new StringBuilder
    for (t <- eventProbabilities) {
      t match {
        case (event, probability) =>
          sb.append(s"Event $event: $probability\n")
      }
    }
    sb.toString
  }

  def *(that: Factor): Factor = {
    forEventsInFactors(intersection(that), that, _ * _, MultiplicativeIdentity)
  }

  def /(that: Factor): Factor = {
    forEventsInFactors(intersection(that), that, _ / _, MultiplicativeIdentity)
  }

  def +(that: Factor): Factor = {
    forEventsInFactors(intersection(that), that, _ + _, AdditiveIdentity)
  }

  def -(that: Factor): Factor = {
    forEventsInFactors(intersection(that), that, _ - _, AdditiveIdentity)
  }

  private def forEventsInFactors(events: Set[Event], that: Factor, op: (Double, Double) => Double, neutral: Factor): Factor = {
    if (Factor.this.equals(neutral)) that
    else if (that.equals(neutral)) this
    else {
      val newEventProbabilities = events map (
        event => (event, op(eventProbabilities(event), that.eventProbabilities(event))))
      Factor(newEventProbabilities.toMap)
    }
  }

  private def intersection(that: Factor): Set[Event] = eventProbabilities.keySet.intersect(that.eventProbabilities.keySet)

  private def union(that: Factor): Set[Event] = eventProbabilities.keySet.union(that.eventProbabilities.keySet)

  def join(that: Factor): Factor = {
    if (Factor.this.equals(JoinIdentity)) that
    else if (that.equals(JoinIdentity)) this
    else {
      val composite = for {
        key1 <- eventProbabilities.keys
        key2 <- that.eventProbabilities.keys
      } yield (key1.union(key2), eventProbabilities(key1) * that.eventProbabilities(key2))
      Factor(composite.toMap)
    }
  }

}

object MultiplicativeIdentity extends Factor(Map[Event, Double]().withDefaultValue(1))

object AdditiveIdentity extends Factor(Map[Event, Double]().withDefaultValue(0))

object JoinIdentity extends Factor

