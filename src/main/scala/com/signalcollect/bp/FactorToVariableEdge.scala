package com.signalcollect.bp

import com.signalcollect.DefaultEdge

class LogFactorToVariableEdge(targetId: String) extends DefaultEdge(targetId) {
  type Source = LogFactorVertex
  // TODO: Support evidence.
  def signal = {
    val distWithoutTarget = source.state - source.signalMap(targetId)
    distWithoutTarget.marginalFor(Variable(targetId))
  }
}

class FactorToVariableEdge(targetId: String) extends DefaultEdge(targetId) {
  type Source = FactorVertex
  // TODO: Support evidence.
  // TODO: Evaluate using sum of logarithms instead. 
  // TODO: Make div by 0 case more efficient and DRY with other edge.
  def signal = {
    val influenceFromTarget = source.signalMap(targetId)
    val divByZero = influenceFromTarget.f.map.valuesIterator.contains(0)
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
      source.state / influenceFromTarget
    }
    distWithoutTargetInfluence.marginalFor(Variable(targetId)).normalize
  }
}
