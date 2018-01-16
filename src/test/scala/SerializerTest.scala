import geometry.Point
import geometry.support.GeometrySupport
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import parser.TaskParser
import serialiser.Serializer

import scala.language.postfixOps


@RunWith(classOf[JUnitRunner])
class SerializerTest extends FunSuite with GeometrySupport with TaskParser {

  test("serialising success") {
    println(new Serializer(List(Point(0, 0), Point(10, 10), Point(12, 12))).serialize)
  }
}
