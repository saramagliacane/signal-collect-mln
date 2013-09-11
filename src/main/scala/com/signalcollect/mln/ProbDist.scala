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

  def product(that: Factor): Factor = {
    if (Factor.this.equals(MultiplicativeIdentity)) {
      that
    } else if (that.equals(MultiplicativeIdentity)) {
      Factor.this
    } else {
      val keysIntersect = eventProbabilities.keySet.intersect(that.eventProbabilities.keySet)
      val newEventProbabilities = keysIntersect.map { key =>
        (key, eventProbabilities(key) * that.eventProbabilities(key))
      }.toMap
      Factor(newEventProbabilities)
    }
  }

  def join(that: Factor): Factor = {
    if (Factor.this.equals(MultiplicativeIdentity)) {
      that
    } else if (that.equals(MultiplicativeIdentity)) {
      Factor.this
    } else {
      val composite = for {
        key1 <- eventProbabilities.keys
        key2 <- that.eventProbabilities.keys
      } yield (key1.union(key2), eventProbabilities(key1) * that.eventProbabilities(key2))
      Factor(composite.toMap)
    }
  }
}

object MultiplicativeIdentity extends Factor

