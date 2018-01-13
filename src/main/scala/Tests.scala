import Algo.Result
import geometry._
import optimum.OptimumParametrisedSearch
import parser.{Task, TaskParser}
import play.api.libs.json.Json

/**
  * Created by k.neyman on 20.11.2017. 
  */
//noinspection SimplifyBoolean
object Tests extends GeometrySupport {
  class CrossDetections {
    def testCrossing() {
      println(geometry.MyVector(Point(0, 0), Point(10, 10)).isCrossedBy(geometry.MyVector(Point(0, 10), Point(10, 0))) == true)
    }

    def testParallel() {
      println(geometry.MyVector(Point(0, 0), Point(10, 10)).isCrossedBy(geometry.MyVector(Point(1, 0), Point(11, 10))) == false)
    }

    def testOneSharedPoint() {
      println(geometry.MyVector(Point(0, 0), Point(10, 10)).isCrossedBy(geometry.MyVector(Point(10, 10), Point(10, 0))) == true)
    }

    def testSelfCheck(): Unit = {
      // Exception
      val result = try {
        println(geometry.MyVector(Point(0, 0), Point(10, 10)).isCrossedBy(geometry.MyVector(Point(0, 0), Point(10, 10))))
        false
      } catch {
        case _: IllegalArgumentException => true
        case _: Throwable => false
      }
      println(result)
    }

    def testHalfBounds(): Unit = {
      println(geometry.MyVector(Point(0, 0), Point(10, 10)).halfContainer.isCrossedBy(geometry.MyVector(Point(10, 0), Point(10, 10))) == false)
    }

    def testContainsPoint(): Unit = {
      println(geometry.MyVector(Point(0, 0), Point(10, 10)).halfContainer.contains(Point(1.378, 1.379)) == true)
    }
  }

  class FigureSupport {
    private def createRhombus: Figure = {
      val points = Point(0, 5) :: Point(7.5, 10) :: Point(15, 5) :: Point(7.5, 0) :: Nil
      Figure.fromVertex(points)
    }

    def testContains(): Unit = {
      println(createRhombus.containsPoint(Point(7.5, 4)) == true)
    }

    def testContainsHorizontalLineCrossVertex(): Unit = {
      println(createRhombus.containsPoint(Point(7.5, 5)) == true)
    }

    def testNotContains(): Unit = {
      println(createRhombus.containsPoint(Point(7.5, 15)) == false)
    }

    def testNotContainsVertex(): Unit = {
      println(createRhombus.vertex.forall(p => createRhombus.containsPoint(p) == false))
    }

    def testNotContainsHorizontal(): Unit = {
      println(createRhombus.containsPoint(Point(0, 10)) == false)
    }

    def testCrossedByVector(): Unit = {
      val box: Figure = Figure.fromVertex(Point(2, 2) :: Point(2, 4) :: Point(4, 4) :: Point(4, 2) :: Nil)

      println(box.findCrossings(MyVector(Point(3, 3), Point(2, 3))) == Point(2, 3) :: Nil)
      println(box.findCrossings(MyVector(Point(2, 3), Point(3, 3))) == Point(2, 3) :: Nil)
      println(box.findCrossings(MyVector(Point(1, 3), Point(2, 3))) == Nil)
      println(box.findCrossings(MyVector(Point(2, 3), Point(1, 3))) == Nil)

      println(box.findCrossings(MyVector(Point(2, 2), Point(3, 3))) == Point(2, 2) :: Nil)
      println(box.findCrossings(MyVector(Point(3, 3), Point(2, 2))) == Point(2, 2) :: Nil)
      println(box.findCrossings(MyVector(Point(1, 1), Point(2, 2))) == Nil)
      println(box.findCrossings(MyVector(Point(2, 2), Point(1, 1))) == Nil)
    }

    def testCrossedByVector2(): Unit = {
      val root = Point(11, 6)
      val finish = Point(14, 9)

      val figure = Figure.fromVertex(Point(7, 6) :: Point(7, 8) :: Point(13, 8) :: Point(13, 4) :: Point(11, 4) :: Point(11, 6) :: Nil)
      val vector: MyVector = MyVector(root, finish)
//      println(barrier2.findCrossings(vector))

      val p = root
      val r1 = vector.contains(p) && vector.containsAsRectangle(p)
//      println(r1)
      val EPS_ACCURACY_CROSS_DETECTION_AT_BOUND = 0.001
//      println(new FigureLineInterception(figure).checkPoint(p, vector))
      val p2: Point = new ParametrizedLine(vector).apply(EPS_ACCURACY_CROSS_DETECTION_AT_BOUND)
      println(p2)
      println(figure.containsPoint(p2))
    }
  }

  class TestTaskParser extends TaskParser {
    def testParse(): Unit = {
      val task: Task = Json.parse(testVariant).as[Task]
      println(task.start == Point(2, 2))
      println(task.finish == Point(48, 28))
      val point: Point = task.polygons.head.vertices.head

      println(point == Point(3, 19))
    }

    private val testVariant = "{\n    \"start\": {\n        \"x\": 2,\n        \"y\": 2\n    },\n    \"finish\": {\n        \"x\": 48,\n        \"y\": 28\n    },\n    \"polygons\": [\n        {\n            \"x\": 3,\n            \"y\": 17,\n            \"vertices\": [\n                {\n                    \"x\": 3,\n                    \"y\": 19\n                },\n                {\n                    \"x\": 4,\n                    \"y\": 18\n                },\n                {\n                    \"x\": 5,\n                    \"y\": 17\n                },\n                {\n                    \"x\": 5,\n                    \"y\": 15\n                },\n                {\n                    \"x\": 3,\n                    \"y\": 13\n                },\n                {\n                    \"x\": 1,\n                    \"y\": 14\n                },\n                {\n                    \"x\": -1,\n                    \"y\": 16\n                },\n                {\n                    \"x\": 0,\n                    \"y\": 18\n                },\n                {\n                    \"x\": 2,\n                    \"y\": 19\n                }\n            ]\n        },\n        {\n            \"x\": 6,\n            \"y\": 27,\n            \"vertices\": [\n                {\n                    \"x\": 6,\n                    \"y\": 30\n                },\n                {\n                    \"x\": 7,\n                    \"y\": 28\n                },\n                {\n                    \"x\": 8,\n                    \"y\": 26\n                },\n                {\n                    \"x\": 7,\n                    \"y\": 25\n                },\n                {\n                    \"x\": 5,\n                    \"y\": 25\n                },\n                {\n                    \"x\": 4,\n                    \"y\": 25\n                },\n                {\n                    \"x\": 1,\n                    \"y\": 27\n                },\n                {\n                    \"x\": 4,\n                    \"y\": 28\n                },\n                {\n                    \"x\": 5,\n                    \"y\": 30\n                }\n            ]\n        }\n    ]\n}"
  }

  class TestMoveAlong extends GeometrySupport {
    def createTriangle(): Figure = {
      val points = Point(0, 5) :: Point(10, 10) :: Point(10, 0) :: Nil
      Figure.fromVertex(points)
    }

    private def createRhombus(): Figure = {
      val points = Point(0, 5) :: Point(7.5, 10) :: Point(15, 5) :: Point(7.5, 0) :: Nil
      Figure.fromVertex(points)
    }

    private def createStrangeFig(): Figure = {
      val points = Point(5, 5) :: Point(2, 10) :: Point(15, 5) :: Point(7.5, 0) :: Nil
      Figure.fromVertex(points)
    }

    def testMoveAlong(): Unit = {
      val triangle = createTriangle()

      def distToEnd(p: Point): Double = MyVector(p, Point(15, 10)).length2
      println(triangle.moveAlongToTarget(Point(0, 5), distToEnd))
    }

    def testMoveAlongLongPath(): Unit = {
      val rombus = createRhombus()

      def distToEnd(p: Point): Double = MyVector(p, Point(20, 5)).length2
      println(rombus.moveAlongToTarget(Point(0, 5), distToEnd))
    }

    def testMoveAlongLongPathOnRightSide(): Unit = {
      val barrier = createStrangeFig()

      def distToEnd(p: Point): Double = MyVector(p, Point(30, 5)).length2
      println(barrier.moveAlongToTarget(Point(5, 5), distToEnd))
    }

    def testNoMove(): Unit = {
      val triangle = createTriangle()

      def distToEnd(p: Point): Double = MyVector(p, Point(-10, 5)).length2
      println(triangle.moveAlongToTarget(Point(0, 5), distToEnd) == Nil)
    }
  }

  class TestSearchWay {
    implicit val accuracy: EPS = 0.1

    def testSimpleWay(): Unit = {
      val barrier = Figure.fromVertex(Point(2, 2) :: Point(2, 5) :: Point(6, 5) :: Point(6, 2) :: Nil)

      val result: Result = new Algo(Field(100, 100), barrier :: Nil, Point(3, 1), Point(6, 7)).solve()
      val ideal = List(
        MyVector(Point(3, 1), Point(3.5, 2)),
        MyVector(Point(3.5, 2), Point(6, 2)),
        MyVector(Point(6, 2), Point(6, 5)),
        MyVector(Point(6, 5), Point(6, 7)))
      println(comparePath(result, ideal))
    }

    def testSimpleWay2(): Unit = {
      val barrier = Figure.fromVertex(Point(2, 2) :: Point(2, 5) :: Point(6, 5) :: Point(6, 2) :: Nil)

      val result: Result = new Algo(Field(100, 100), barrier :: Nil, Point(3, 1), Point(9, 4)).solve()
      val ideal = List(
        MyVector(Point(3, 1), Point(5, 2)),
        MyVector(Point(5, 2), Point(6, 2)),
        MyVector(Point(6, 2), Point(6, 4)),
        MyVector(Point(6, 4), Point(9, 4)))
      println(comparePath(result, ideal))
    }

    def testSimpleWayBadNums(): Unit = {
      val barrier = Figure.fromVertex(Point(2, 2) :: Point(2, 5) :: Point(6, 5) :: Point(6, 2) :: Nil)

      val result: Result = new Algo(Field(100, 100), barrier :: Nil, Point(3, 1), Point(7, 4)).solve()
      val ideal = makePath(List(Point(3, 1), Point(4 + (1.0 / 3), 2), Point(6, 2), Point(6, 4), Point(7, 4)))
      println(toPoints(result))
      println(comparePath(result, ideal))
    }

    def testSearchWithDeadEnd(): Unit = {
      val root = Point(2, 1)
      val finish = Point(14, 9)

      val barrier1 = Figure.fromVertex(Point(3, 2) :: Point(3, 5) :: Point(8, 5) :: Point(8, 2) :: Nil)
      val barrier2 = Figure.fromVertex(Point(7, 6) :: Point(7, 8) :: Point(13, 8) :: Point(13, 4) :: Point(11, 4) :: Point(11, 6) :: Nil)

      val result: Result = new Algo(Field(100, 100), barrier1 :: barrier2 :: Nil, root, finish).solve()
      val ideal =  makePath(List(root, Point(3.5, 2), Point(8, 2), Point(8, 5), Point(9.5, 6), Point(11, 6)))

      println(toPoints(result))
      println(comparePath(result, ideal))
    }
  }

  private def makePath(points: List[Point]): List[MyVector] = {
    (points zip points.tail).map { case (p1, p2) => MyVector(p1, p2) }
  }

  private def toPoints(vectors: List[MyVector]): List[Point] = vectors.head.from :: vectors.map(_.to)

  type EPS = Double
  private def comparePath(actual: Result, ideal: Result)(implicit rangeToPoint: EPS): Boolean = {
    val range2 = math.pow(rangeToPoint, 2)
    def inRange(p1: Point, p2: Point): Boolean = MyVector(p1, p2).length2 < range2
    if (actual.size == ideal.size) {
      if (actual.isEmpty) true
      else {
        val zipped = toPoints(actual) zip toPoints(ideal)
        zipped.forall { case (p1, p2) => inRange(p1, p2) }
      }
    } else {
      false
    }
  }

  class OptimiserTest {
    def testSearch(): Unit = {
      val target = Point(9, 4)
      def optimum(p: Point): Double = MyVector(p, target).length2
      val paramEdge = new ParametrizedLine(MyVector(Point(6, 2), Point(6, 5)))
      def optimumFun(t: Double): Double = { optimum(paramEdge(t)) }
      println(new OptimumParametrisedSearch(optimumFun).find - (2.0 / 3) < 0.05)
    }
  }
}
