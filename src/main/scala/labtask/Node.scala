package labtask

import geometry.{MyVector, Point}

/**
  * Created by Neikila on 14.01.2018.
  */
class Node(val point: Point) {
  var neighbours: List[NodeNeighbor] = Nil
  var isConnectedToStart = false
  var isConnectedToFinish = false

  private var _parentToStart: Option[Node] = None
  private var _parentToFinish: Option[Node] = None

  def parentToStart_=(node: Node): Unit = {
    if (_parentToStart.isEmpty) _parentToStart = Some(node)
  }

  def parentToFinish_=(node: Node): Unit = {
    if (_parentToFinish.isEmpty) _parentToFinish = Some(node)
  }

  def parentToStart: Option[Node] = _parentToStart
  def parentToFinish: Option[Node] = _parentToFinish

  def addNeighbor(node: Node, path: => List[MyVector]): Unit = {
    if (!neighbours.exists(_.nodeNeighbor == node)) {
      neighbours = NodeNeighbor(node, path) :: neighbours
    }
  }

  override def equals(other: Any): Boolean = other match {
    case that: Node => point == that.point
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(point)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString: String = {
    s"point = $point neighbors = ${neighbours.map(_.nodeNeighbor.point)} parentStart = ${parentToStart.map(_.point)} parentFinish = ${parentToFinish.map(_.point)}"
  }
}

object Node {
  def root(point: Point): Node = {
    val node = new Node(point)
    node.isConnectedToStart = true
    node
  }

  def finish(point: Point): Node = {
    val node = new Node(point)
    node.isConnectedToFinish = true
    node
  }
}