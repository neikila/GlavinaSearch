import Algo.Result
import geometry.{Figure, GeometrySupport, MyVector, Point}

/**
  * Created by k.neyman on 20.11.2017. 
  */
class Algo(val field: Field, val barriers: List[Figure], val globalFrom: Point, val globalTo: Point)
  extends GeometrySupport {

  val EPS_INTERCEPTION_DIFFERENCE = 0.01

  var extraPoints: List[Point] = Nil

  def solve(): Result = {
    getToClosestToTarget(globalFrom, globalTo)
  }

  def getToClosestToTarget(from: Point, to: Point): Result = {
    val straightLineToFinish = MyVector(from, to)
    findInterception(straightLineToFinish) match {
      case Some(interception) if MyVector(interception.interceptionPoint, from).length2 < EPS_INTERCEPTION_DIFFERENCE =>
        Nil
      case Some(interception) =>
        val toBarrier = MyVector(straightLineToFinish.from, interception.interceptionPoint)

        def distanceToEnd(p: Point): Double = distance2(p, straightLineToFinish.to)
        val movedAround = interception.barrier.moveAlongToTarget(interception.interceptionPoint, distanceToEnd)
        val allPath = toBarrier :: movedAround

        val closest: Point = allPath.last.to

        if (closest == straightLineToFinish.to) allPath
        else allPath ::: getToClosestToTarget(closest, to)

      case _ => straightLineToFinish :: Nil
    }
  }

  private case class Interception(interceptedPath: MyVector, barrier: Figure, interceptionPoint: Point)

  private def findInterception(v: MyVector): Option[Interception] = {
    def distToStart(point: Point): Double = distance2(v.from, point)

    val interceptions: Stream[Interception] = barriers.toStream.flatMap { b =>
      val iterable: Iterable[Point] = b.findCrossings(v)
      if (iterable.isEmpty) None
      else Some(Interception(v, b, iterable.minBy(distToStart)))
    }

    if (interceptions.isEmpty) None
    else Some(interceptions.minBy { case Interception(_, _, p) => distToStart(p) })
  }

  private def distance2(p1: Point, p2: Point): Double = {
    math.pow(p1.x - p2.x, 2) + math.pow(p1.y - p2.y, 2)
  }
}

object Algo {
  type Result = List[MyVector]
}
