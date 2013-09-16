package com.signalcollect.bp

import com.signalcollect._

/**
 * The vertex representing the factors.
 * @param id
 * @param initialState
 */
class LogFactorVertex(id: String,
                      initialState: Distribution,
                      utility: Distribution)
    extends DataGraphVertex(id, initialState) {
  type Signal = Distribution
  def collect = signals.reduce(_ + _)
  def signalMap = mostRecentSignalMap
}

class FactorVertex(id: String,
                   initialState: Distribution,
                   utility: Distribution)
    extends DataGraphVertex(id, initialState) {
  type Signal = Distribution
  def collect = Distribution((signals.reduce(_ * _)).f * utility.f)
  def signalMap = mostRecentSignalMap
}
