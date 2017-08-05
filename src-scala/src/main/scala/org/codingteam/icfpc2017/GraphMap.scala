package org.codingteam.icfpc2017

import org.codingteam.icfpc2017.Common.Punter
import org.codingteam.icfpc2017.GameMap._

import scalax.collection.edge.LUnDiEdge
import scalax.collection.mutable.Graph
import scalax.collection.edge.LBase.LEdgeImplicits

case class GraphMap(var graph: Graph[Node, LUnDiEdge]) {

  object PunterImplicit extends LEdgeImplicits[Option[Punter]]
  import PunterImplicit._

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

  def removeEdge(source: Node, target: Node) : Unit = {
    val g = graph
    val sourceNode = g get source
    val targetNode = g get target
    assert(sourceNode.hasSuccessor(targetNode))

    val edge = sourceNode.connectionsWith(targetNode).head
    graph -= edge
  }

  def distance(source: Node, target: Node): Int = {
    val g = graph
    val sourceNode = g get source
    val targetNode = g get target

    (sourceNode shortestPathTo targetNode) match {
      case None => 0
      case Some(path) => path.edges.size
    }
  }

  def getFreeEdges() : Iterable[Graph[Node, LUnDiEdge]#EdgeT] = {
    graph.edges.filter {
      edge: Graph[Node, LUnDiEdge]#EdgeT => edge.label == None
    }
  }

  def getPunterSubgraph(punter : Punter) : GraphMap = {
    val g = graph
    val newGraph = g filter g.having(edge = {
      edge: g.EdgeT => (edge.label != None) && (edge.label == punter)
    })
    GraphMap(newGraph)
  }

  def punterDistance(punter : Punter, source: Node, target: Node) : Int = {
    getPunterSubgraph(punter).distance(source, target)
  }

  def test(): Unit = {
    val g = graph
    for (c <- g.componentTraverser()) {
      println(c.nodes)
    }
  }
}

object GraphMap {
  def fromMap(map: Map): GraphMap = {
    GraphMap(map.toGraph())
  }
}
