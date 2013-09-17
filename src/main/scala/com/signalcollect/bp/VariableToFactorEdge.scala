package com.signalcollect.bp

import com.signalcollect.DefaultEdge

class LogVariableToFactorEdge[Id](targetId: Id) extends DefaultEdge(targetId) {
  type Source = LogVariableVertex
  def signal = {
    val targetInfluence = source.signalMap.get(targetId)
    if (targetInfluence.isDefined) {
      source.state - targetInfluence.get
    } else {
      source.state
    }
  }
}

class VariableToFactorEdge[Id](targetId: Id) extends DefaultEdge(targetId) {
  type Source = VariableVertex
  // TODO: Add normalization.
  // TODO: Evaluate using sum of logarithms instead.
  // TODO: Make div by 0 case more efficient and DRY with other edge.
  def signal = {
    val influenceFromTarget = source.signalMap.get(targetId)
    if (influenceFromTarget.isDefined) {
      val divByZero = influenceFromTarget.get.f.map.valuesIterator.contains(0)
      if (divByZero) {
        source.signals.foldLeft(JoinIdentity: Distribution) {
          case (acc, next) =>
            if (next != influenceFromTarget) {
              acc * next
            } else {
              acc
            }
        }
      } else {
        (source.state / influenceFromTarget.get)
      }
    } else {
      source.state
    }
  }
}
