import java.io.{BufferedWriter, File, FileWriter}

import geometry.{Figure, MyVector, Point}
import geometry.support.GeometrySupport
import labtask.{AccuracySettings, Algo, Field, Interception}
import labtask.Algo.Result
import parser.{Task, TaskParser}
import play.api.libs.json.Json
import serialiser.Serializer

import scala.io.Source

/**
  * Created by k.neyman on 17.11.2017. 
  */
object Main extends GeometrySupport with TaskParser {
  implicit val accuracySettings: AccuracySettings = new AccuracySettings

  def main(args: Array[String]): Unit = {
    val str = Source.fromFile("C:\\Users\\Neikila\\Documents\\study\\GlavinaSearch\\res\\source.json").getLines().mkString
    val task = Json.parse(str).as[Task]
    val barriers = task.polygons.map(fig => Figure.fromVertices(fig.vertices))

    val root = task.start
    val finish = task.finish
    val field = Field(task.finish.x + 2, task.finish.y + 2)

    val result = toPoints(new Algo(field, barriers, root, finish).solve())
    println(result)
    printToFile(new Serializer(result).serialize)
  }

  def printToFile(result: String): Unit = {
    val file = new File("C:\\Users\\Neikila\\Documents\\study\\GlavinaSearch\\res\\result.json")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(result)
    bw.close()
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














