package com.signalcollect.bp

import com.signalcollect._

/**
 * The vertex representing the factors.
 * @param id
 * @param initialState
 */
class FactorVertex(id: Int, initialState: Distribution = JoinIdentity)
    extends DataGraphVertex(id, initialState) {
  type Signal = Distribution
  def collect = signals.reduce(_ + _)
  def signalMap = mostRecentSignalMap
}

//class FactorVertex(id: Int, initialState: Distribution = JoinIdentity)
//    extends DataGraphVertex(id, initialState) {
//  type Signal = Distribution
//  def collect = signals.reduce(_ * _)
//  def signalMap = mostRecentSignalMap
//}
