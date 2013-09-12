package com.signalcollect.mln

import com.signalcollect._

object Test extends App {
	val g = GraphBuilder.build
	val factor1 = Factor().addValue(Set("a"), 0.5)
	val dist1 = Distribution().addValue(Set("b"), 0.6)
	g.addVertex(new FactorVertex(1, factor1))
	g.addVertex(new VariableVertex(2, dist1))
	println(g.execute)
	g.foreachVertex(println)
	g.shutdown
}
