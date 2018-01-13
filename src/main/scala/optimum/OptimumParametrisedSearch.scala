package optimum

/**
  * Created by k.neyman on 21.11.2017. 
  */
class OptimumParametrisedSearch(val optimiser: Double => Double) {
  private val dividersAmountFirstStep: Int = 10
  private val dividersAmountSecondStep: Int = 10

  def find: Double = {
    val d1 = delta(0, 1, dividersAmountFirstStep)
    val t1 = roundTo(find(0, 1, d1), dividersAmountFirstStep)
    val d2 = delta(math.max(0, t1 - d1), math.min(1, t1 + d1), dividersAmountSecondStep)
    roundTo(find(math.max(0, t1 - d1), math.min(1, t1 + d1), d2), dividersAmountFirstStep * dividersAmountSecondStep)
  }

  private def delta(from: Double, to: Double, dividersAmount: Int) = (to - from) / dividersAmount

  private def roundTo(num: Double, accuracy: Double): Double = math.round(num * accuracy) / accuracy

  private def find(from: Double, to: Double, delta: Double): Double = {
    (for (t <- from to to by delta) yield t).minBy(optimiser)
  }
}
