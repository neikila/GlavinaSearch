import geometry.{GeometrySupport, MyVector, ParametrizedLine, Point}
import optimum.OptimumParametrisedSearch
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.language.postfixOps


@RunWith(classOf[JUnitRunner])
class OptimiserTest extends FunSuite with GeometrySupport {

  test("search success") {
    val target = Point(9, 4)

    def optimum(p: Point): Double = MyVector(p, target).length2
    val paramEdge = new ParametrizedLine(MyVector(Point(6, 2), Point(6, 5)))
    def optimumFun(t: Double): Double = { optimum(paramEdge(t)) }

    assert(new OptimumParametrisedSearch(optimumFun).find - (2.0 / 3) < 0.05)
  }
}
