package geometry.support

import geometry._
import labtask.AccuracySettings
import optimum.OptimumParametrisedSearch

/**
  * Created by Neikila on 14.01.2018.
  */
trait MoveAlongSupport extends FigureInterceptSupport {
  private implicit val accuracy: MyVectorAccuracy.ContainsAccuracy = new AccuracySettings().VECTOR_CONTAIN_ACCURACY
  private implicit val accuracySettings: AccuracySettings = new AccuracySettings()

  type OptFun = Point => Double
  implicit class MoveAlongFigure(val figure: Figure) {
    def moveAlongToTarget(from: Point, optimim: OptFun, otherBarriers: List[Figure]): List[MyVector] = {
      val edge = getInitEdge(from)
      val (rightPart, leftPart) = edge.splitBy(from)

      val (left, (_ :: right)) = figure.lines.span(_ != edge)
      val circle = right ::: left

      val leftCircle =
        if (leftPart.isZeroVector) circle
        else leftPart :: circle

      val rightCircle =
        (if (rightPart.isZeroVector) circle.reverse
        else rightPart :: circle.reverse).map(_.reverse)

      val minLeft = moveRound(from, cutPath(leftCircle, otherBarriers), optimim)
      val minRight = moveRound(from, cutPath(rightCircle, otherBarriers), optimim)
      selectMinPath(from, minLeft, minRight)(optimim)
    }

    private def cutPath(path: List[MyVector], otherBarriers: List[Figure]): List[MyVector] = {
      path.toStream
        .map { vector => vector -> vector.findInterception(otherBarriers) }
        .span { case (_, optInterception) => optInterception.isEmpty } match {
        case (noInterception, (lastPart, Some(interception)) #:: _) =>
          noInterception.map { case (v, _) => v }.toList :+ MyVector(lastPart.from, interception.interceptionPoint)
        case (noInterception, _) => noInterception.map { case (v, _) => v }.toList
      }
    }

    private def selectMinPath(from: Point,
                              pathLeft: List[MyVector],
                              pathRight: List[MyVector])
                             (implicit optimum: OptFun): List[MyVector] = {
      val initVal = optimum(from)
      (pathLeft, pathRight) match {
        case (Nil, Nil) => Nil
        case (Nil, _) =>
          if (result(pathRight) < initVal) optimise(pathRight)
          else Nil
        case (_, Nil) =>
          if (result(pathLeft) < initVal) optimise(pathLeft)
          else Nil
        case (_, _) =>
          val left: Double = result(pathLeft)
          val right: Double = result(pathRight)
          if (left < initVal || right < initVal) {
            if (left < right) optimise(pathLeft)
            else optimise(pathRight)
          } else {
            Nil
          }
      }
    }

    private def optimise(path: List[MyVector]): List[MyVector] = {
      path.filterNot(v => v.from == v.to)
    }

    private def result(list: List[MyVector])(implicit optimim: OptFun): Double = {
      optimim(list.last.to)
    }

    private def moveRound(from: Point, lines: List[MyVector], optimim: OptFun): List[MyVector] = {
      lines match {
        case head :: tail =>
          val paramLine = new ParametrizedLine(head)
          val tMin = moveAlongEdge(paramLine, optimim)
          val nextP = paramLine(tMin)

          if (tMin == 1) MyVector(from, nextP) :: moveRound(nextP, tail, optimim)
          else MyVector(from, nextP) :: Nil
        case Nil => Nil
      }
    }

    private def moveAlongEdge(paramEdge: ParametrizedLine, optimim: OptFun): Double = {
      def optimumFun(t: Double): Double = { optimim(paramEdge(t)) }
      new OptimumParametrisedSearch(optimumFun).find
    }

    private def getInitEdge(from: Point): MyVector = {
      if (figure.vertices.contains(from)) {
        figure.vertexToLines(from).right
      } else {
        figure.lines.find(vector => vector.contains(from))
          .getOrElse(throw new IllegalArgumentException("Point not on edge of figure"))
      }
    }
  }
}
