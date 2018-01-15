package geometry.support

import geometry._
import labtask.AccuracySettings
import optimum.OptimumParametrisedSearch

/**
  * Created by Neikila on 14.01.2018.
  */
trait MoveAlongSupport {
  private implicit val accuracy: MyVectorAccuracy.ContainsAccuracy = new AccuracySettings().VECTOR_CONTAIN_ACCURACY

  type OptFun = Point => Double
  implicit class MoveAlongFigure(val figure: Figure) {
    def moveAlongToTarget(from: Point, optimim: OptFun): List[MyVector] = {
      val edge = getInitEdge(from)

      val (left, (_ :: right)) = figure.lines.span(_ != edge)
      val circle = right ::: left

      val minLeft = moveRound(from, edge :: circle, optimim, reversed = false)
      val minRight = moveRound(from, edge :: circle.reverse, optimim, reversed = true)
      selectMinPath(from, minLeft, minRight)(optimim)
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

    private def moveRound(from: Point, lines: List[MyVector], optimim: OptFun, reversed: Boolean): List[MyVector] = {
      val tValToGoNext = if (reversed) 0 else 1
      lines match {
        case head :: tail =>
          val paramLine = new ParametrizedLine(head)
          val tMin = moveAlongEdge(paramLine, optimim)
          val nextP = paramLine(tMin)
          if (tMin == tValToGoNext)
            MyVector(from, nextP) :: moveRound(nextP, tail, optimim, reversed)
          else
            MyVector(from, nextP) :: Nil
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
