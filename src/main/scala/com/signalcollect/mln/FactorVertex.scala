package com.signalcollect.mln

import com.signalcollect._

/**
 * The vertex representing the factors.
 * @param id
 * @param initialState
 */
class FactorVertex[Id](id: Id, initialState: Distribution)
    extends DataGraphVertex(id, initialState) {
  type Signal = Distribution
  def collect = signals.reduce(_ * _)
}
