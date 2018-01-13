package geometry

/**
  * Created by k.neyman on 21.11.2017. 
  */
class ParametrizedLine(val edge: MyVector) {
  def apply(t: Double): Point = Point(
    f(t, edge.from.x, edge.to.x),
    f(t, edge.from.y, edge.to.y)
  )

  private def f(t: Double, x1: Double, x2: Double): Double = {
    (1 - t) * x1 + t * x2
  }
}
