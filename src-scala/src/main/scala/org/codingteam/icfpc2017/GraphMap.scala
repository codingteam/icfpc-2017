package org.codingteam.icfpc2017

import org.codingteam.icfpc2017.Common.Punter
import org.codingteam.icfpc2017.GameMap._

import scalax.collection.edge.LUnDiEdge
import scalax.collection.mutable.Graph

case class GraphMap(var graph: Graph[Node, LUnDiEdge]) {

  def getNodes: Iterable[Node] = graph.nodes

  def getMines: Iterable[Mine] = getNodes.collect { case mine: Mine => mine }

  def getMineNodes: Iterable[Graph[Node, LUnDiEdge]#NodeT] = {
    graph.nodes.filter {
      node: Graph[Node, LUnDiEdge]#NodeT => node.value.isInstanceOf[Mine]
    }
  }

  def getSiteNodes: Iterable[Graph[Node, LUnDiEdge]#NodeT] = {
    graph.nodes.filter {
      node: Graph[Node, LUnDiEdge]#NodeT => node.value.isInstanceOf[Site]
    }
  }

  def mark(source: Node, target: Node, punter: Punter): Unit = {
    val g = graph
    val sourceNode = g get source
    val targetNode = g get target
    assert(sourceNode.hasSuccessor(targetNode))

    val edge = sourceNode.connectionsWith(targetNode).head
    graph -= edge

    implicit val factory = scalax.collection.edge.LDiEdge
    graph.addLEdge(source, target)(punter)
  }
}

object GraphMap {
  def fromMap(map: Map): GraphMap = {
    GraphMap(map.toGraph())
  }
}
