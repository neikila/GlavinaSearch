package geometry

/**
  * Created by k.neyman on 20.11.2017. 
  */

case class MyVector(from: Point, to: Point) {
  private val EPS = 0.1

  lazy val line: Line = Line(
    from.y - to.y,
    to.x - from.x,
    from.x * to.y - to.x * from.y
  )

  def contains(p: Point): Boolean = {
    val l1 = MyVector(from, p)
    val l2 = MyVector(p, to)

    math.abs(l1.x * l2.y - l1.y * l2.x) < EPS
  }

  def x: Double = to.x - from.x
  def y: Double = to.y - from.y

  def length2: Double = math.pow(x, 2) + math.pow(y, 2)

  def containsAsRectangle(point: Point): Boolean = {
    val left = math.min(from.x, to.x)
    val right = math.max(from.x, to.x)

    val top = math.max(from.y, to.y)
    val bottom = math.min(from.y, to.y)

    left - EPS <= point.x && point.x <= right + EPS &&
      bottom - EPS <= point.y && point.y <= top + EPS
  }

  def nonBounded: MyVector = new NotBoundedMyVector(from, to)
}

class NotBoundedMyVector(from: Point, to: Point) extends MyVector(from, to) {

  override def contains(p: Point): Boolean = {
    if (p == from || p == to) false
    else super.contains(p)
  }

  override def containsAsRectangle(point: Point): Boolean = {
    if (point == from || point == to) {
      false
    } else {
      super.containsAsRectangle(point)
    }
  }
}

