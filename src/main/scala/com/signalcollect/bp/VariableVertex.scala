package com.signalcollect.bp

import com.signalcollect._

/**
 * The vertex representing the variables.
 * @param id
 * @param initialState
 */
class LogVariableVertex(id: String, initialState: Distribution)
  extends DataGraphVertex(id, initialState) {
  type Signal = Distribution
  def collect = signals.reduce(_ + _)
  def signalMap = mostRecentSignalMap
}

class VariableVertex(id: String, initialState: Distribution)
  extends DataGraphVertex(id, initialState) {
  type Signal = Distribution
  def collect = signals.reduce(_ * _)
  def signalMap = mostRecentSignalMap
}
