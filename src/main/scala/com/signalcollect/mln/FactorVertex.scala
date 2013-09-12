package com.signalcollect.mln

import com.signalcollect._

/**
 * The vertex representing the factors.
 * @param id
 * @param initialState
 */
class FactorVertex[Id](id: Id, initialState: Factor[Int]) extends DataGraphVertex(id, initialState) {
  type Signal = Factor[Int]
  def collect = signals.foldLeft(MultiplicativeIdentity.asInstanceOf[Factor[Int]])(_ * _)
}
