package com.signalcollect.bp

import com.signalcollect._

/**
 * The vertex representing the variables.
 * @param id
 * @param initialState
 */
class LogVariableVertex(id: String,
                        initialState: Distribution,
                        val isEvidence: Boolean)
    extends DataGraphVertex(id, initialState) {
  type Signal = Distribution
  def collect = signals.reduce(_ + _)
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
