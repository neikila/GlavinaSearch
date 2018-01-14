import geometry.Point
import geometry.support.GeometrySupport
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import parser.{Task, TaskParser}
import play.api.libs.json.Json

import scala.language.postfixOps


@RunWith(classOf[JUnitRunner])
class ParserTest extends FunSuite with GeometrySupport with TaskParser {

  private val testVariant = "{\n    \"start\": {\n        \"x\": 2,\n        \"y\": 2\n    },\n    \"finish\": {\n        \"x\": 48,\n        \"y\": 28\n    },\n    \"polygons\": [\n        {\n            \"x\": 3,\n            \"y\": 17,\n            \"vertices\": [\n                {\n                    \"x\": 3,\n                    \"y\": 19\n                },\n                {\n                    \"x\": 4,\n                    \"y\": 18\n                },\n                {\n                    \"x\": 5,\n                    \"y\": 17\n                },\n                {\n                    \"x\": 5,\n                    \"y\": 15\n                },\n                {\n                    \"x\": 3,\n                    \"y\": 13\n                },\n                {\n                    \"x\": 1,\n                    \"y\": 14\n                },\n                {\n                    \"x\": -1,\n                    \"y\": 16\n                },\n                {\n                    \"x\": 0,\n                    \"y\": 18\n                },\n                {\n                    \"x\": 2,\n                    \"y\": 19\n                }\n            ]\n        },\n        {\n            \"x\": 6,\n            \"y\": 27,\n            \"vertices\": [\n                {\n                    \"x\": 6,\n                    \"y\": 30\n                },\n                {\n                    \"x\": 7,\n                    \"y\": 28\n                },\n                {\n                    \"x\": 8,\n                    \"y\": 26\n                },\n                {\n                    \"x\": 7,\n                    \"y\": 25\n                },\n                {\n                    \"x\": 5,\n                    \"y\": 25\n                },\n                {\n                    \"x\": 4,\n                    \"y\": 25\n                },\n                {\n                    \"x\": 1,\n                    \"y\": 27\n                },\n                {\n                    \"x\": 4,\n                    \"y\": 28\n                },\n                {\n                    \"x\": 5,\n                    \"y\": 30\n                }\n            ]\n        }\n    ]\n}"

  test("parsing success") {
    val task: Task = Json.parse(testVariant).as[Task]
    assert(task.start == Point(2, 2))
    assert(task.finish == Point(48, 28))
    val point: Point = task.polygons.head.vertices.head

    assert(point == Point(3, 19))
  }
}
