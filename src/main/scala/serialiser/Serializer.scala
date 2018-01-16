package serialiser

import geometry.{MyVector, Point}
import play.api.libs.json._

/**
  * Created by Neikila on 16.01.2018.
  */
class Serializer(val points: List[Point]) {
  implicit val pointWrites = new Writes[Point] {
    def writes(point: Point): JsObject = {
      Json.obj("cx" -> point.x, "cy" -> point.y)
    }
  }

  implicit val vectorsWrite = new Writes[MyVector] {
    def writes(v: MyVector): JsObject = {
      Json.obj(
        "x1" -> v.from.x,
        "y1" -> v.from.y,
        "x2" -> v.to.x,
        "y2" -> v.to.y
      )
    }
  }

  def serialize: String = {
    val jsPoints: JsValue = Json.toJson(points)
    val jsVectors: JsValue = Json.toJson((points zip points.drop(1)).map { case (left, right) => MyVector(left, right) })
    Json.prettyPrint(jsPoints.as[JsArray]  ++ jsVectors.as[JsArray])
  }
}
