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
    //println(s"mark: $source $target $punter")
    val sourceNode = g get source
    val targetNode = g get target
    assert(sourceNode.hasSuccessor(targetNode))

    val edge = sourceNode.connectionsWith(targetNode).head
    graph -= edge

    implicit val factory = scalax.collection.edge.LUnDiEdge
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
    assert(! g.isEmpty)
    if (source == target) {
      0
    } else {
      // println(s"Distance from $source to $target, graph: $g")
      if ((g.nodes.contains(source)) && (g.nodes.contains(target))) {
        val sourceNode = g get source
        val targetNode = g get target

        (sourceNode shortestPathTo targetNode) match {
          case None => 0
          case Some(path) => path.edges.size
        }
      } else {
        0
      }
    }
  }

  def hasPath(source: Node, target: Node) : Boolean = {
    val g = graph
    if ((g.nodes.contains(source)) && (g.nodes.contains(target))) {
      val sourceNode = g get source
      val targetNode = g get target

      (sourceNode pathTo targetNode) match {
        case None => false
        case _ => true
      }
    } else {
      false
    }
  }

  def getFreeEdges() : Iterable[Graph[Node, LUnDiEdge]#EdgeT] = {
    graph.edges.filter {
      edge: Graph[Node, LUnDiEdge]#EdgeT => edge.label == None
    }
  }

  def getFreeSubgraph() : GraphMap = {
    val g = graph
    val newGraph = g filter g.having(edge = {
      edge: g.EdgeT => edge.label == None
    })
    GraphMap(newGraph)
  }

  def getPunterEdges(punter : Punter) : Iterable[Graph[Node, LUnDiEdge]#EdgeT] = {
    val g = graph
    g.edges.filter {
      edge: g.EdgeT => (edge.label != None) && (edge.label == punter)
    }
  }

  def getPunterNeighbours(punter : Punter) : Iterable[Graph[Node, LUnDiEdge]#EdgeT] = {
    val punterEdges = getPunterEdges(punter)
    val free = getFreeEdges
    if (punterEdges.isEmpty) {
      List()
    } else {
      val punterNodes = punterEdges.flatMap({
        edge: Graph[Node, LUnDiEdge]#EdgeT => edge.nodes
      }).toSet
      val g = graph
      free.filterNot {
        edge: Graph[Node, LUnDiEdge]#EdgeT => edge.nodes.toSet.intersect(punterNodes).isEmpty
      }
    }
  }

  def getFreeNearMines() : Iterable[Graph[Node, LUnDiEdge]#EdgeT] = {
    val free = getFreeEdges
    val mines = getMineNodes.toSet

    free.filterNot {
      edge: Graph[Node, LUnDiEdge]#EdgeT => edge.nodes.toSet.intersect(mines).isEmpty
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

  def scoreMineSite(punter : Punter, mine : Node, site : Node) : Int = {
    if (getPunterSubgraph(punter).hasPath(mine, site)) {
      val d = distance(mine, site)
      d * d
    } else {
      0
    }
  }

  def scoreMine(punter: Punter, mine: Node): Int = {
    val g = graph
    g.nodes.map({
      node: Graph[Node, LUnDiEdge]#NodeT => scoreMineSite(punter, mine, node.value)
    }).sum
  }

  def score(punter : Punter) : Int = {
    getMineNodes.map({
      node: Graph[Node, LUnDiEdge]#NodeT => scoreMine(punter, node.value)
    }).sum
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
