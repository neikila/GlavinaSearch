import geometry.{Figure, MyVector, Point}
import geometry.support.GeometrySupport
import geometry.task.AccuracySettings

/**
  * Created by k.neyman on 17.11.2017. 
  */
object Main extends GeometrySupport {
  implicit val accuracySettings: AccuracySettings = new AccuracySettings

  def main(args: Array[String]): Unit = {
    val barrier1 = Figure.fromVertices(Point(3, 2) :: Point(3, 5) :: Point(8, 5) :: Point(8, 2) :: Nil)
    val root = Point(2, 1)
    val finish = Point(14, 9)

    val vector = MyVector(root, finish)
    val p = barrier1.vertices.head
//    vector.contains(p)
    println(barrier1.findCrossingWithVertex(vector))
  }


  trait Generator[T] {
    def generate: T
  }
}














