package com.signalcollect.mln

import com.signalcollect._

/**
 * The vertex representing the variables.
 * @param id
 * @param initialState
 */
class VariableVertex[Id](id: Id, initialState: Factor) extends DataGraphVertex(id, initialState) {
  type Signal = Factor
  def collect = signals.foldLeft(JoinIdentity: Factor)(_ join _)
}
