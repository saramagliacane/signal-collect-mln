package com.signalcollect.bp

import com.signalcollect._
import scala.collection.parallel.immutable.WithDefault

/**
 * The vertex representing the variables.
 * @param id
 * @param initialState
 */
class LogVariableVertex(
  id: String,
  initialState: Map[String, Distribution] = Map.withDefaultValue(JoinIdentity),
  val isEvidence: Boolean)
    extends DataGraphVertex(id, initialState) {
  type Signal = Distribution
  def collect = {
    val left = signals.toArray
    val right = left.reverse
    //    for (i <- (0 to left.length)) {
    //      
    //    }
    var acc: Distribution = JoinIdentity
    val accFromLeft = left map { signal =>
      acc = signal * acc
      acc
    }
    acc = JoinIdentity
    val accFromLeft = left map { signal =>
      acc = signal * acc
      acc
    }
    signals.reduce(_ + _)
  }
  def signalMap = mostRecentSignalMap
}

class VariableVertex(id: String,
                     initialState: Distribution,
                     isEvidence: Boolean)
    extends DataGraphVertex(id, initialState) {
  type Signal = Distribution
  def collect = if (isEvidence) initialState else signals.reduce(_ * _)
  def signalMap = mostRecentSignalMap
}
