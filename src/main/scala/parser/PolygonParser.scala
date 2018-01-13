package parser

import geometry.Point
import play.api.libs.functional.syntax._
import play.api.libs.json._

/**
  * Created by k.neyman on 29.11.2017. 
  */
trait PolygonConstants extends PointConstants {
  val VERTICES = "vertices"
}

case class FigurePlain(x: Double, y : Double, vertices: List[Point])

trait PolygonParser extends PolygonConstants with PointParser {
  implicit val polygonRead: Reads[FigurePlain] =
    ((JsPath \ X).read[Double] and
      (JsPath \ Y).read[Double] and
      (JsPath \ VERTICES).read[List[Point]]
      ) (FigurePlain.apply _)
}
