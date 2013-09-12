package com.signalcollect.mln

import com.signalcollect._

/**
 * The vertex representing the variables.
 * @param id
 * @param initialState
 */
class VariableVertex[Id](id: Id, initialState: Distribution[Int]) extends DataGraphVertex(id, initialState) {
  type Signal = Distribution[Int]
  def collect = signals.foldLeft(JoinIdentity.asInstanceOf[Distribution[Int]])(_ join _)
}
