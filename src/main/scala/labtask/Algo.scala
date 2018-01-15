package labtask

import geometry.support.GeometrySupport
import labtask.Algo.Result
import geometry.{Figure, MyVector, Point}

/**
  * Created by k.neyman on 20.11.2017.
  */
class Algo(val field: Field, val barriers: List[Figure], val globalFrom: Point, val globalTo: Point)
  extends GeometrySupport {

  private implicit val accuracySettings: AccuracySettings = new AccuracySettings

  private val pointGen = new PointGen(field)
  private val searchGraph = new SearchGraph(globalFrom, globalTo, accuracySettings)

  def solve(): Result = {
    recursive(globalFrom, globalTo)
    if (!searchGraph.hasFoundWay) {
      withRandomPoints()
    }
    searchGraph.getPath
  }

  private def recursive(from: Point, to: Point): Unit = {
    findPathAndAddSubTarget(from, to) match {
      case Some((node, _)) =>
        val points: List[Point] = getOpposite(node)
        points.foreach { p => recursive(p, node.point) }
      case _ => None
    }
  }

  private def getOpposite(node: Node): List[Point] = {
    if (node.isConnectedToStart) {
      getFinishTreePoints
    } else {
      getStartTreePoints
    }
  }

  private def getFinishTreePoints = {
    globalTo :: searchGraph.getFinishSubTargets
  }

  private def getStartTreePoints = {
    globalFrom :: searchGraph.getStartSubTargets
  }

  private def getAllSearchTarget: List[Point] = {
    globalFrom :: globalTo :: searchGraph.getSubTargets
  }

  private def withRandomPoints(): Unit = {
    do {
      val nextTarget = generatePoint

      println()
      println(s"Generate new point: $nextTarget")
      println()

      getAllSearchTarget.foreach { t =>
        println(s"From $t to $nextTarget")
        recursive(t, nextTarget)
      }
    } while (!searchGraph.hasFoundWay)
  }

  private def findPathAndAddSubTarget(from: Point, to: Point): Option[(Node, Result)] = {
    val path = getToClosestToTarget(from, to)
    if (path.nonEmpty) {
      val point = lastPoint(path)
      val (node, isNewNode) = searchGraph.connect(point, from, path)
      if (isNewNode) {
        println(s"New node $node")
        println(s"path = $path\n")
        Some(node, path)
      } else {
        println(s"Old node $node\n")
        None
      }
    } else {
      None
    }
  }

  def getToClosestToTarget(from: Point, to: Point): Result = {
    val straightLineToFinish = MyVector(from, to)
    straightLineToFinish.findInterception(barriers) match {
      case Some(interception) if isDeadEnd(straightLineToFinish.from, interception) => Nil
      case Some(interception) =>
        val toBarrier = MyVector(straightLineToFinish.from, interception.interceptionPoint)
        toBarrier :: moveAroundBarrier(straightLineToFinish, interception)
      case _ => straightLineToFinish :: Nil
    }
  }

  private def moveAroundBarrier(straightLineToFinish: MyVector, interception: Interception) = {
    def distanceToEnd(p: Point): Double = MyVector(p, straightLineToFinish.to).length2

    println(s"Vector = $straightLineToFinish interceptionPoint ${interception.interceptionPoint} ")
    val movedAround = interception.barrier.moveAlongToTarget(interception.interceptionPoint, distanceToEnd, barriers.filter(_ != interception.barrier))
    if (movedAround.nonEmpty) {
      val closest: Point = lastPoint(movedAround)

      if (closest.isApproximatelyEqual(straightLineToFinish.to)(accuracySettings.POINT_ACCURACY_EQUAL)) movedAround
      else movedAround ::: getToClosestToTarget(closest, straightLineToFinish.to)
    } else {
      Nil
    }
  }

  private def isDeadEnd(from: Point, interception: Interception): Boolean = {
    MyVector(interception.interceptionPoint, from).length2 < accuracySettings.POINT_ACCURACY_EQUAL
  }

  private def generatePoint: Point = {
    var point: Point = pointGen.generate
    while (barriers.exists(_.containsPoint(point))) {
      point = pointGen.generate
    }
    point
  }

  private def lastPoint(result: Result): Point = result.last.to

}

case class Interception(interceptedPath: MyVector, barrier: Figure, interceptionPoint: Point)

object Algo {
  type Result = List[MyVector]
}
