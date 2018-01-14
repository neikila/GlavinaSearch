package geometry.task

import scala.util.Random

/**
  * Created by Neikila on 14.01.2018.
  */
class DoubleGenExclusive(val range: Double) {
  private val rand = new Random()

  def generate: Double = {
    rand.nextDouble() match {
      case 0 => generate
      case 1 => generate
      case v => v * range
    }
  }
}
