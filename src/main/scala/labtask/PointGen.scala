package labtask

import geometry.Point

/**
  * Created by Neikila on 14.01.2018.
  */
class PointGen(val field: Field) {
  private val widthGen = new DoubleGenExclusive(field.width)
  private val heightGen = new DoubleGenExclusive(field.height)

  def generate: Point = {
    Point(widthGen.generate, heightGen.generate)
  }
}


