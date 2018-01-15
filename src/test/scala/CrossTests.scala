import geometry.{MyVectorAccuracy, Point}
import geometry.support.GeometrySupport
import labtask.AccuracySettings
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.language.postfixOps


@RunWith(classOf[JUnitRunner])
class CrossTests extends FunSuite with GeometrySupport {
  implicit val accuracy: MyVectorAccuracy.ContainsAccuracy = new AccuracySettings().VECTOR_CONTAIN_ACCURACY
  private val vector = geometry.MyVector(Point(0, 0), Point(10, 10))

  test("testCrossing") {
    assert(vector.isCrossedBy(geometry.MyVector(Point(0, 10), Point(10, 0))))
  }

  test("testParallel") {
    assert(!vector.isCrossedBy(geometry.MyVector(Point(1, 0), Point(11, 10))))
  }

  test("testOneSharedPoint") {
    assert(vector.isCrossedBy(geometry.MyVector(Point(10, 10), Point(10, 0))))
  }

  test("testSelfCheck") {
    assert(!vector.isCrossedBy(vector))
  }

  test("contains point nearby") {
    assert(vector.contains(Point(1.378, 1.379)))
  }

  test("test contains nearby outside vertex") {
    assert(vector.contains(Point(10.001, 10.001)))
  }

  test("test nonbound doesnt contain nearby outside vertex") {
    assert(!vector.nonBounded.contains(Point(10.001, 10.001)))
  }
}
