package com.signalcollect.bp

sealed trait Variable {
  def name: String
  def boundTo(value: Any) = new BoundVariable(name, value)
  def unbound = new UnboundVariable(name)
}

case class UnboundVariable(name: String) extends Variable

case class BoundVariable(name: String, value: Any) extends Variable {
  override def toString = s"$name = $value"
}

object Variable {
  def apply(name: String) = new UnboundVariable(name)
  def apply(name: String, value: Any) = new BoundVariable(name, value)
}
