import geometry.{GeometrySupport, Point}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.language.postfixOps


@RunWith(classOf[JUnitRunner])
class CrossTests extends FunSuite with GeometrySupport {

  test("testCrossing") {
    assert(geometry.MyVector(Point(0, 0), Point(10, 10)).isCrossedBy(geometry.MyVector(Point(0, 10), Point(10, 0))))
  }

  test("testParallel") {
    assert(!geometry.MyVector(Point(0, 0), Point(10, 10)).isCrossedBy(geometry.MyVector(Point(1, 0), Point(11, 10))))
  }

  test("testOneSharedPoint") {
    assert(geometry.MyVector(Point(0, 0), Point(10, 10)).isCrossedBy(geometry.MyVector(Point(10, 10), Point(10, 0))))
  }

  test("testSelfCheck") {
    // Exception
    val result = try {
      geometry.MyVector(Point(0, 0), Point(10, 10)).isCrossedBy(geometry.MyVector(Point(0, 0), Point(10, 10)))
      false
    } catch {
      case _: IllegalArgumentException => true
      case _: Throwable => false
    }
    assert(result)
  }

  test("testHalfBounds") {
    assert(!geometry.MyVector(Point(0, 0), Point(10, 10)).halfContainer.isCrossedBy(geometry.MyVector(Point(10, 0), Point(10, 10))))
  }

  test("testContainsPoint") {
    assert(geometry.MyVector(Point(0, 0), Point(10, 10)).halfContainer.contains(Point(1.378, 1.379)))
  }
}
