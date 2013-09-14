package com.signalcollect.mln

import com.signalcollect._

/**
 * The vertex representing the variables.
 * @param id
 * @param initialState
 */
class VariableVertex[Id](id: Id, initialState: Distribution)
    extends DataGraphVertex(id, initialState) {
  type Signal = Distribution
  def collect = signals.reduce(_ * _)
}
