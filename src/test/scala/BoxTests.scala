import geometry._
import geometry.support.GeometrySupport
import labtask.AccuracySettings
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.language.postfixOps


@RunWith(classOf[JUnitRunner])
class BoxTests extends FunSuite with GeometrySupport {
  private implicit val accuracySettings: AccuracySettings = new AccuracySettings

  private val box: Figure = Figure.fromVertices(Point(2, 2) :: Point(2, 4) :: Point(4, 4) :: Point(4, 2) :: Nil)

  private val center = Point(3, 3)
  private val leftLineMid = Point(2, 3)
  private val rightLineMid = Point(4, 3)
  private val boxVertex = Point(2, 2)

  private val outsideHorizontal = Point(1, 3)
  private val outsideDiagonal = Point(1, 1)

  test("Crossed Line inside of box") {
    assert(box.findCrossings(MyVector(center, leftLineMid)) == leftLineMid :: Nil)
    assert(box.findCrossings(MyVector(leftLineMid, center)) == leftLineMid :: Nil)
  }

  test("Crossed Line outside of box") {
    assert(box.findCrossings(MyVector(outsideHorizontal, leftLineMid)) == Nil)
    assert(box.findCrossings(MyVector(leftLineMid, outsideHorizontal)) == Nil)
  }

  test("Crossed Vertex inside of box") {
    assert(box.findCrossings(MyVector(boxVertex, center)) == boxVertex :: Nil)
    assert(box.findCrossings(MyVector(center, boxVertex)) == boxVertex :: Nil)
  }

  test("Crossed Vertex outside of box") {
    assert(box.findCrossings(MyVector(outsideDiagonal, boxVertex)) == Nil)
    assert(box.findCrossings(MyVector(boxVertex, outsideDiagonal)) == Nil)
  }

  test("Crossed box through") {
    assert(box.findCrossings(MyVector(boxVertex, rightLineMid)) == boxVertex :: rightLineMid :: Nil)
  }
}
