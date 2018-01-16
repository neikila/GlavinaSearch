package geometry

import geometry.support.ParametrizedLineSupport

/**
  * Created by k.neyman on 20.11.2017. 
  */

case class MyVector(from: Point, to: Point) extends ParametrizedLineSupport {

  // Attributes
  lazy val line: Line = Line(
    from.y - to.y,
    to.x - from.x,
    from.x * to.y - to.x * from.y
  )

  def x: Double = to.x - from.x
  def y: Double = to.y - from.y

  def length2: Double = math.pow(x, 2) + math.pow(y, 2)
  def length: Double = math.sqrt(math.pow(x, 2) + math.pow(y, 2))

  // Geometric checks
  def isZeroVector: Boolean = from == to

  def contains(p: Point)(implicit accuracy: MyVectorAccuracy.ContainsAccuracy): Boolean = {
    isVertex(p) || containsInside(p)
  }

  protected def isVertex(p: Point)(implicit accuracy: MyVectorAccuracy.ContainsAccuracy): Boolean = {
    MyVector(from, p).length2 < accuracy || MyVector(to, p).length2 < accuracy
  }

  protected def containsInside(p: Point)(implicit accuracy: MyVectorAccuracy.ContainsAccuracy): Boolean = {
    val param: TParam = this.findProjectionT(p)
    param > 0 && param < 1 && MyVector(parametrized(param), p).length2 < accuracy
  }

  // Math operations
  def *(myVector: MyVector): Double = asRadiusPoint * myVector.asRadiusPoint

  // Transformations
  def nonBounded: MyVector = new NotBoundedMyVector(from, to)
  def bounded: MyVector = MyVector(from, to)
  def asRadiusPoint: Point = Point(x, y)
  def parametrized: ParametrizedLine = new ParametrizedLine(this)
  def splitBy(point: Point): (MyVector, MyVector) = (MyVector(from, point), MyVector(point, to))
  def reverse: MyVector = MyVector(to, from)
}

class NotBoundedMyVector(from: Point, to: Point) extends MyVector(from, to) {
  override def contains(p: Point)(implicit accuracy: MyVectorAccuracy.ContainsAccuracy): Boolean = {
    !isVertex(p) && containsInside(p)
  }
}

object MyVectorAccuracy {
  type ContainsAccuracy = Double
}
