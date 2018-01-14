import geometry.{Figure, MyVector, Point}
import geometry.support.GeometrySupport
import geometry.task.{AccuracySettings, Algo, Field, Interception}
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

//    println(findInterception(MyVector(Point(11.0, 4.2), Point(13.0, 4.2)), barrier2 :: Nil))

    val result: Result = new Algo(Field(15, 15), barrier1 :: barrier2 :: Nil, root, finish).solve()
    println(toPoints(result))
  }

  private def toPoints(vectors: List[MyVector]): List[Point] = vectors.head.from :: vectors.map(_.to)

  private def findInterception(v: MyVector, barriers: List[Figure]): Option[Interception] = {
    def distToStart(point: Point): Double = MyVector(v.from, point).length2

    barriers.flatMap { b =>
      val iterable: Iterable[Point] = b.findCrossings(v)
      if (iterable.isEmpty) None
      else Some(Interception(v, b, iterable.minBy(distToStart)))
    } match {
      case Nil => None
      case interceptions => Some(interceptions.minBy { case Interception(_, _, p) => distToStart(p) })
    }
  }

  private def makePath(points: List[Point]): List[MyVector] = {
    (points zip points.tail).map { case (p1, p2) => MyVector(p1, p2) }
  }
}














