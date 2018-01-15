package labtask

import geometry.MyVectorAccuracy
import geometry.Point.PointAccuracyEqual
import geometry.support.FigureInterceptAccuracy.BoundaryPointDetectionAccuracy

/**
  * Created by Neikila on 14.01.2018.
  */
class AccuracySettings {
  val POINT_ACCURACY_EQUAL: PointAccuracyEqual = 0.01
  val EPS_ACCURACY_CROSS_DETECTION_AT_BOUND: BoundaryPointDetectionAccuracy = 0.3

  private val EPS = 0.1
  val VECTOR_CONTAIN_ACCURACY: MyVectorAccuracy.ContainsAccuracy = EPS * EPS
}

