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

    val field = Field(task.finish.x + 2, task.finish.y + 2)
    val barriers = task.polygons.map(fig => Figure.fromVertices(fig.vertices)) ::: createSideBarriers(field)

    val result = toPoints(new Algo(field, barriers, task.start, task.finish).solve())

    println(result)
    printToFile(new Serializer(result).serialize)
  }

  private def createSideBarriers(field: Field): List[Figure] = {
    val delta = 10
    val zeroPoint = Point(0, 0)
    val fieldPoint = Point(field.width, field.height)
    List(
      Figure.rect(zeroPoint, fieldPoint.copy(y = -delta)),
      Figure.rect(fieldPoint, Point(0, fieldPoint.x + delta)),
      Figure.rect(fieldPoint, Point(0, fieldPoint.y + delta)),
      Figure.rect(zeroPoint, fieldPoint.copy(x = -delta))
    )
  }

  def printToFile(result: String): Unit = {
    val file = new File("C:\\Users\\Neikila\\Documents\\study\\GlavinaSearch\\res\\result.json")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(result)
    bw.close()
  }

  private def toPoints(vectors: List[MyVector]): List[Point] = vectors.head.from :: vectors.map(_.to)
}














