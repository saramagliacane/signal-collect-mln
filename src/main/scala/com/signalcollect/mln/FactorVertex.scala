package com.signalcollect.mln

import com.signalcollect._

/**
 * The vertex representing the factors.
 * @param id
 * @param initialState
 */
class FactorVertex[Id](id: Id, initialState: Factor) extends DataGraphVertex(id, initialState) {
  type Signal = Factor
  def collect = signals.foldLeft(MultiplicativeIdentity: Factor)(_.product(_))
}