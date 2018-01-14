import geometry.{MyVectorAccuracy, Point}
import geometry.support.GeometrySupport
import geometry.task.AccuracySettings
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.language.postfixOps


@RunWith(classOf[JUnitRunner])
class CrossTests extends FunSuite with GeometrySupport {
  implicit val accuracy: MyVectorAccuracy.ContainsAccuracy = new AccuracySettings().VECTOR_CONTAIN_ACCURACY
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

  test("contains point nearby") {
    assert(geometry.MyVector(Point(0, 0), Point(10, 10)).contains(Point(1.378, 1.379)))
  }

  test("test contains nearby outside vertex") {
    assert(geometry.MyVector(Point(0, 0), Point(10, 10)).contains(Point(10.001, 10.001)))
  }

  test("test nonbound doesnt contain nearby outside vertex") {
    assert(!geometry.MyVector(Point(0, 0), Point(10, 10)).nonBounded.contains(Point(10.001, 10.001)))
  }
}
