package parser

import geometry.Point
import play.api.libs.functional.syntax._
import play.api.libs.json._

/**
  * Created by k.neyman on 29.11.2017. 
  */
trait PointConstants {
  val X = "x"
  val Y = "y"
}

trait PointParser extends PointConstants {
  implicit val pointWrites = new Writes[Point] {
    def writes(point: Point): JsObject = {
      Json.obj(X -> point.x, Y -> point.y)
    }
  }

  implicit val pointRead: Reads[Point] =
    ((JsPath \ X).read[Double] and
      (JsPath \ Y).read[Double]
      ) (Point.apply _)
}
