import Algo.Result
import geometry.support.GeometrySupport
import geometry.task.AccuracySettings
import geometry.{Figure, MyVector, Point}

/**
  * Created by k.neyman on 20.11.2017.
  */
class Algo(val field: Field, val barriers: List[Figure], val globalFrom: Point, val globalTo: Point)
  extends GeometrySupport {

  private implicit val accuracySettings: AccuracySettings = new AccuracySettings

  var extraPoints: List[Point] = Nil

  def solve(): Result = {
    getToClosestToTarget(globalFrom, globalTo)
  }

  def getToClosestToTarget(from: Point, to: Point): Result = {
    val straightLineToFinish = MyVector(from, to)
    findInterception(straightLineToFinish) match {
      case Some(interception) if isDeadEnd(straightLineToFinish.from, interception) => Nil
      case Some(interception) =>
        val toBarrier = MyVector(straightLineToFinish.from, interception.interceptionPoint)
        toBarrier :: moveAroundBarrier(straightLineToFinish, interception)
      case _ => straightLineToFinish :: Nil
    }
  }

  private def moveAroundBarrier(straightLineToFinish: MyVector, interception: Interception) = {
    def distanceToEnd(p: Point): Double = MyVector(p, straightLineToFinish.to).length2

    val movedAround = interception.barrier.moveAlongToTarget(interception.interceptionPoint, distanceToEnd)
    val closest: Point = lastPoint(movedAround)

    if (closest.isApproximatelyEqual(straightLineToFinish.to)(accuracySettings.POINT_ACCURACY_EQUAL)) movedAround
    else movedAround ::: getToClosestToTarget(closest, straightLineToFinish.to)
  }

  private def isDeadEnd(from: Point, interception: Interception): Boolean = {
    MyVector(interception.interceptionPoint, from).length2 < accuracySettings.POINT_ACCURACY_EQUAL
  }

  private def findInterception(v: MyVector): Option[Interception] = {
    def distToStart(point: Point): Double = MyVector(v.from, point).length2

    barriers.toStream.flatMap { b =>
      val iterable: Iterable[Point] = b.findCrossings(v)
      if (iterable.isEmpty) None
      else Some(Interception(v, b, iterable.minBy(distToStart)))
    } match {
      case Stream.Empty => None
      case interceptions => Some(interceptions.minBy { case Interception(_, _, p) => distToStart(p) })
    }
  }

  private def lastPoint(result: Result): Point = result.last.to

  private case class Interception(interceptedPath: MyVector, barrier: Figure, interceptionPoint: Point)
}

object Algo {
  type Result = List[MyVector]
}
