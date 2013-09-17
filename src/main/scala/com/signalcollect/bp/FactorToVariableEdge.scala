package com.signalcollect.bp

import com.signalcollect.DefaultEdge

class LogFactorToVariableEdge(targetId: String) extends DefaultEdge(targetId) {
  type Source = LogFactorVertex
  def signal = {
    val targetInfluence = source.signalMap.get(targetId)
    if (targetInfluence.isDefined) {
      (source.state - targetInfluence.get).marginalFor(Variable(targetId))
    } else {
      source.state.marginalFor(Variable(targetId))
    }
  }
}

class FactorToVariableEdge(targetId: String) extends DefaultEdge(targetId) {
  type Source = FactorVertex
  // TODO: Add normalization.
  // TODO: Support evidence.
  // TODO: Evaluate using sum of logarithms instead. 
  // TODO: Make div by 0 case more efficient and DRY with other edge.
  def signal = {
    val influenceFromTarget = source.signalMap.get(targetId)
    if (influenceFromTarget.isDefined) {
      val divByZero = influenceFromTarget.get.f.map.valuesIterator.contains(0)
      val distWithoutTargetInfluence = if (divByZero) {
        source.signals.foldLeft(JoinIdentity: Distribution) {
          case (acc, next) =>
            if (next != influenceFromTarget) {
              acc * next
            } else {
              acc
            }
        }
      } else {
        source.state / influenceFromTarget.get
      }
      distWithoutTargetInfluence.marginalFor(Variable(targetId))
    } else {
      source.state.marginalFor(Variable(targetId))
    }
  }
}
