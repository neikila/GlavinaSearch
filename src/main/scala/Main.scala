import geometry.{Figure, MyVector, Point}
import geometry.support.GeometrySupport
import geometry.task.{AccuracySettings, Algo, Field}
import geometry.task.Algo.Result

/**
  * Created by k.neyman on 17.11.2017. 
  */
object Main extends GeometrySupport {
  implicit val accuracySettings: AccuracySettings = new AccuracySettings

  def main(args: Array[String]): Unit = {
    val root = Point(2, 1)
    val finish = Point(14, 9)

    val barrier1 = Figure.fromVertices(Point(3, 2) :: Point(3, 5) :: Point(8, 5) :: Point(8, 2) :: Nil)
    val barrier2 = Figure.fromVertices(Point(7, 6) :: Point(7, 8) :: Point(13, 8) :: Point(13, 4) :: Point(11, 4) :: Point(11, 6) :: Nil)

    val result: Result = new Algo(Field(100, 100), barrier1 :: barrier2 :: Nil, root, finish).solve()
    val ideal = makePath(List(root, Point(3.5, 2), Point(8, 2), Point(8, 5), Point(9.5, 6), Point(11, 6)))

    println(result)
    println(ideal)
  }

  private def makePath(points: List[Point]): List[MyVector] = {
    (points zip points.tail).map { case (p1, p2) => MyVector(p1, p2) }
  }
}














