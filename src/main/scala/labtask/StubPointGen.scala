package labtask

import geometry.Point

/**
  * Created by Neikila on 16.01.2018.
  */
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
