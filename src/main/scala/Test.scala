import ProbDist._

object Test extends App {
  val dist = uniform(1 to 5)
  val extended = dist.addEvent(10,0.5)
  println(extended)
  println(extended.normalize)
}
