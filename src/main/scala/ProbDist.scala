import scala.util.Random
import scala.annotation.tailrec

case class ProbDist[Event](
  eventProbabilities: Map[Event, Double] = Map[Event, Double]())
  extends Function1[Event, Double] {

  def addEvent(e: Event, probability: Double): ProbDist[Event] = {
    assert(probability >= 0)
    val newProbabilities = eventProbabilities + ((e, probability))
    ProbDist[Event](newProbabilities)
  }

  def apply(e: Event): Double = {
    eventProbabilities.get(e).getOrElse(0)
  }

  def normalize: ProbDist[Event] = {
    if (!isNormalized) {
      val probabilitySum = eventProbabilities.values.sum
      val newProbabilities = {
        if (probabilitySum == 0) {
          val uniformEventProbability = 1.0 / eventProbabilities.size
          eventProbabilities map {
            case (event, _) =>
              (event, uniformEventProbability)
          }
        } else {
          eventProbabilities map {
            case (event, unnormalizedProbability) =>
              (event, unnormalizedProbability / probabilitySum)
          }
        }
      }
      ProbDist[Event](newProbabilities)
    } else {
      this
    }
  }

  /**
   * Draws an event from this distribution.
   */
  def draw: Event = {
    val point = Random.nextDouble * sum
    var currentSum = 0.0
    for (event <- eventProbabilities.keys) {
      currentSum += eventProbabilities(event)
      if (currentSum >= point) {
        return event
      }
    }
    throw new Exception("Invalid distribution.")
  }

  def isNormalized = math.abs(sum - 1.0) < 0.000001

  private lazy val sum = eventProbabilities.values.sum

  /**
   * Returns an event stream that follows the distribution.
   * TODO: Make immune to stackoverflow.
   */
  def stream: Stream[Event] = {
    draw #:: stream
  }

  override def toString = {
    val sb = new StringBuilder
    for (t <- eventProbabilities) {
      t match {
        case (event, probability) =>
          sb.append(s"Event $event: $probability\n")
      }
    }
    sb.toString
  }

}

object ProbDist {
  def uniform[E](events: Traversable[E]): ProbDist[E] = {
    val uniformEventProbability = 1.0 / events.size
    val probabilities = events map ((_, uniformEventProbability))
    ProbDist(probabilities.toMap)
  }
}
