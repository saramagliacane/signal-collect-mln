package com.signalcollect.bp

import org.scalatest._
import Distribution._
import org.scalatest.prop.Checkers
import com.signalcollect._

class BeliefPropagationSpec
    extends FlatSpec
    with ShouldMatchers
    with Checkers {

  val aTrueDist = bernoulli("a", 1)
  val bUnknownDist = bernoulli("b", 0.5)
  val implicationDist = bernoulli("a", 1) -> bernoulli("b", 1)

  val g = GraphBuilder.build
  val varA = new VariableVertex("a", aTrueDist, isEvidence = true)
  val varB = new VariableVertex("b", bUnknownDist, isEvidence = false)
  g.addVertex(varA)
  g.addVertex(varB)
  val aThenB = new FactorVertex("aThenB", JoinIdentity, implicationDist)
  g.addVertex(aThenB)
  g.addEdge("aThenB", new FactorToVariableEdge("a"))
  g.addEdge("aThenB", new FactorToVariableEdge("b"))
  g.addEdge("a", new VariableToFactorEdge("aThenB"))
  g.addEdge("b", new VariableToFactorEdge("aThenB"))
  g.execute
  g.foreachVertex(println)
}
