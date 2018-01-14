package geometry.task

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

class StubPointGen(field: Field) extends PointGen(field) {
  private var counter = -1
  private val points = (Point(68.83935584307345,40.010119672246816) :: Nil).toArray

  override def generate: Point = {
    counter += 1
    if (counter == points.length) {
      counter -= 1
    }
    points(counter)
  }
}
