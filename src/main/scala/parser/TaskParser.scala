package parser

import geometry.Point
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads}

/**
  * Created by k.neyman on 29.11.2017. 
  */
trait TaskConstants {
  val START_POINT = "start"
  val FINISH_POINT = "finish"
  val POLYGONS = "polygons"
}

case class Task(start: Point, finish: Point, polygons: List[FigurePlain])

trait TaskParser extends TaskConstants with PolygonParser with PointParser {
  implicit val taskRead: Reads[Task] =
    ((JsPath \ START_POINT).read[Point] and
      (JsPath \ FINISH_POINT).read[Point] and
      (JsPath \ POLYGONS).read[List[FigurePlain]]
      ) (Task.apply _)
}
