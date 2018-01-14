package geometry.support

import geometry.{MyVector, Point}

/**
  * Created by Neikila on 14.01.2018.
  */
trait ParametrizedLineSupport {
  type TParam = Double

  implicit class ParamSupportExtension(val myVector: MyVector) {
    def findProjectionT(point: Point): TParam = {
      MyVector(myVector.from, point) * myVector / myVector.length2
    }

    def findProjection(point: Point): Point = {
      myVector.parametrized.apply(findProjectionT(point))
    }
  }
}
