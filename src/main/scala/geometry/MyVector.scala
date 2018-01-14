package geometry

import geometry.support.ParametrizedLineSupport

/**
  * Created by k.neyman on 20.11.2017. 
  */

case class MyVector(from: Point, to: Point) extends ParametrizedLineSupport {
  private val EPS = 0.1
  private val EPS2 = EPS * EPS

  // Attributes
  lazy val line: Line = Line(
    from.y - to.y,
    to.x - from.x,
    from.x * to.y - to.x * from.y
  )

  def x: Double = to.x - from.x
  def y: Double = to.y - from.y

  def length2: Double = math.pow(x, 2) + math.pow(y, 2)

  // Geometric checks
  def contains(p: Point): Boolean = {
    isVertex(p) || containsInside(p)
  }

  protected def isVertex(p: Point): Boolean = {
    MyVector(from, p).length2 < EPS2 || MyVector(to, p).length2 < EPS2
  }

  protected def containsInside(p: Point): Boolean = {
    val param: TParam = this.findProjectionT(p)
    param > 0 && param < 1 && MyVector(parametrized(param), p).length2 < EPS2
  }

  // Math operations
  def *(myVector: MyVector): Double = asRadiusPoint * myVector.asRadiusPoint

  // Transformations
  def nonBounded: MyVector = new NotBoundedMyVector(from, to)
  def asRadiusPoint: Point = Point(x, y)
  def parametrized: ParametrizedLine = new ParametrizedLine(this)
}

class NotBoundedMyVector(from: Point, to: Point) extends MyVector(from, to) {
  override def contains(p: Point): Boolean = containsInside(p)
}

