import geometry._
import geometry.support.{GeometrySupport, ParametrizedLineSupport}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.language.postfixOps


@RunWith(classOf[JUnitRunner])
class ParametrisedLineSupportTest extends FunSuite with ParametrizedLineSupport {
  test("Search protection T positive") {
    val vector = MyVector(Point(0, 0), Point(10, 10))
    val point = Point(20, 20)

    assert(vector.findProjectionT(point) == 2)
  }

  test("Search protection T negative") {
    val vector = MyVector(Point(0, 0), Point(10, 10))
    val point = Point(-20, -20)

    assert(vector.findProjectionT(point) == -2)
  }

  test("Search protection T inside") {
    val vector = MyVector(Point(0, 0), Point(10, 10))
    val point = Point(5, 5)

    assert(vector.findProjectionT(point) == 0.5)
  }

  test("Search protection T outside small delay") {
    val vector = MyVector(Point(0, 0), Point(10, 10))
    val point = Point(10.001, 10.001)

    assert(vector.findProjectionT(point) > 1)
  }
}
