package geometry.task

import geometry.{MyVector, Point}
import geometry.Point.PointAccuracyEqual

/**
  * Created by Neikila on 14.01.2018.
  */
class SearchGraph(startPoint: Point, endPoint: Point, accuracySettings: AccuracySettings) {
  implicit val POINT_ACCURACY_EQUAL: PointAccuracyEqual = accuracySettings.POINT_ACCURACY_EQUAL

  private val startNode = Node.root(startPoint)
  private val endNode = Node.finish(endPoint)

  private var subTargets: List[Node] = Nil

  var connectionNode: Option[Node] = None
  def hasFoundWay: Boolean = connectionNode.isDefined
  def getPath: List[MyVector] = new ResultPathBuilder().getPath

  def getSubTargets: List[Point] = subTargets.map(_.point)
  def getStartSubTargets: List[Point] = subTargets.filter(_.parentToStart.isDefined).map(_.point)
  def getFinishSubTargets: List[Point] = subTargets.filter(_.parentToFinish.isDefined).map(_.point)

  def isStartTree(point: Point): Boolean = findNode(point).isConnectedToStart

  def connect(target: Point, achievedFrom: Point, path: List[MyVector]): (Node, Boolean) = {
    val fromNode = findNode(achievedFrom)

    val (nodeToConnect, isNew) = findNodeOpt(target) match {
      case Some(p) => (p, false)
      case None =>
        val newNode = new Node(target)
        subTargets = newNode :: subTargets
        (newNode, true)
    }

    connect(fromNode, nodeToConnect, path)
    if (fromNode.isConnectedToStart) nodeToConnect.parentToStart = fromNode
    if (fromNode.isConnectedToFinish) nodeToConnect.parentToFinish = fromNode
    if (new ConnectionStatusUpdater(fromNode, nodeToConnect).update() && !hasFoundWay) {
      connectionNode = Some(nodeToConnect)
    }
    (nodeToConnect, isNew)
  }

  private def findNode(point: Point): Node = {
    findNodeOpt(point).getOrElse(throw new IllegalArgumentException(s"No node near $point"))
  }

  private def findNodeOpt(point: Point): Option[Node] = {
    (startNode :: endNode :: subTargets)
      .find(_.point.isApproximatelyEqual(point))
  }

  private def connect(node1: Node, node2: Node, pathFrom1To2: List[MyVector]): Unit = {
    node1.addNeighbor(node2, pathFrom1To2)
    node2.addNeighbor(node1, reversePath(pathFrom1To2))
  }

  private def reversePath(path: List[MyVector]): List[MyVector] = {
    path.reverse.map { case MyVector(from, to) => MyVector(to, from) }
  }

  private class ResultPathBuilder {
    def getPath: List[MyVector] = {
      val path = buildPathToStart ::: buildPathToFinish.reverse
      (path zip path.drop(1)).flatMap { case (left, right) => left.neighbours.find(_.nodeNeighbor == right).get.pathToNeighbor }
    }

    private def buildPathToStart = {
      var lastNode = connectionNode.get.parentToStart
      var pathToStart: List[Node] = Nil
      do {
        pathToStart = lastNode.get :: pathToStart
        lastNode = lastNode.get.parentToStart
      } while (lastNode.isDefined)
      pathToStart
    }

    private def buildPathToFinish = {
      var lastNode: Option[Node] = connectionNode
      var pathToFinish: List[Node] = Nil
      do {
        pathToFinish = lastNode.get :: pathToFinish
        lastNode = lastNode.get.parentToFinish
      } while (lastNode.isDefined)
      pathToFinish
    }
  }
}








