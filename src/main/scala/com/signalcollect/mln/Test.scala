package com.signalcollect.mln

import com.signalcollect._

object Test extends App {
	val g = GraphBuilder.build
	val factor1 = Factor().addEvent(Event(Set("a")), 0.5)
	val factor2 = Factor().addEvent(Event(Set("b")), 0.6)
	g.addVertex(new FactorVertex(1, factor1))
	g.addVertex(new VariableVertex(2, factor2))
	println(g.execute)
	g.foreachVertex(println)
	g.shutdown
}
