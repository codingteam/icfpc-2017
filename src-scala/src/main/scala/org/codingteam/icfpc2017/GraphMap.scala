package org.codingteam.icfpc2017

import org.codingteam.icfpc2017.Common.Punter
import org.codingteam.icfpc2017.GameMap._
import org.codingteam.icfpc2017.futures.Future

import scala.collection.mutable.{ListBuffer, Map => MMap, Set => MSet}
import scalax.collection.edge.LUnDiEdge
import scalax.collection.mutable.Graph
import scalax.collection.edge.LBase.LEdgeImplicits

case class GraphMap(var graph: Graph[Node, LUnDiEdge]) extends Logging {

  object PunterImplicit extends LEdgeImplicits[Punter]

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
    assert(sourceNode.hasSuccessor(targetNode), s"$sourceNode should have $targetNode successor")

    val edge = sourceNode.connectionsWith(targetNode).head
    graph -= edge

    implicit val factory = scalax.collection.edge.LUnDiEdge
    graph.addLEdge(source, target)(punter)
  }

  def removeEdge(source: Node, target: Node): Unit = {
    val g = graph
    val sourceNode = g get source
    val targetNode = g get target
    assert(sourceNode.hasSuccessor(targetNode))

    val edge = sourceNode.connectionsWith(targetNode).head
    graph -= edge
  }

  def distanceUncached(source: Node, target: Node): Int = {
    val g = graph
    assert(!g.isEmpty)
    if (source == target) {
      0
    } else {
      // println(s"Distance from $source to $target, graph: $g")
      if ((g.contains(source)) && (g.contains(target))) {
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

  private var distanceCache: MMap[(Node, Node), Int] = MMap.empty

  def getDistanceCacheCopy(): MMap[(Node, Node), Int] = {
    return distanceCache.clone()
  }

  def setDisatanceCache(cache: MMap[(Node, Node), Int]): Unit = {
    distanceCache = cache
  }

  def distance(source: Node, target: Node): Int = {
    distanceCache.get((source, target)) match {
      case Some(d) => d
      case None => {
        val d = distanceUncached(source, target)
        //println(s"Cache miss: distance($source, $target) = $d")
        distanceCache.put((source, target), d)
        d
      }
    }
  }

  def hasPath(source: Node, target: Node): Boolean = {
    val g = graph
    if ((g.contains(source)) && (g.contains(target))) {
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

  def getFreeEdges(): Iterable[Graph[Node, LUnDiEdge]#EdgeT] = {
    graph.edges.filter {
      edge: Graph[Node, LUnDiEdge]#EdgeT => edge.label == None
    }
  }

  def getFreeSubgraph(): GraphMap = {
    val g = graph
    val newGraph = g filter g.having(edge = {
      edge: g.EdgeT => edge.label == None
    })
    GraphMap(newGraph)
  }

  def getPunterEdges(punter: Punter): Iterable[Graph[Node, LUnDiEdge]#EdgeT] = {
    val g = graph
    g.edges.filter {
      edge: g.EdgeT => (edge.label != None) && (edge.label == punter)
    }
  }

  def getForeignEdges(punter: Punter): Iterable[Graph[Node, LUnDiEdge]#EdgeT] = {
    val g = graph
    g.edges.filter {
      edge: g.EdgeT => (edge.label != None) && (edge.label != punter)
    }
  }

  def getNeighbours(startEdges: Set[Graph[Node, LUnDiEdge]#EdgeT]): Iterable[Graph[Node, LUnDiEdge]#EdgeT] = {
    if (startEdges.isEmpty) {
      List()
    } else {
      val result: MSet[Graph[Node, LUnDiEdge]#EdgeT] = MSet()
      startEdges.foreach({ edge: Graph[Node, LUnDiEdge]#EdgeT =>
        edge.nodes.foreach({ node: Graph[Node, LUnDiEdge]#NodeT =>
          node.edges.foreach({ neighbour: Graph[Node, LUnDiEdge]#EdgeT =>
            val label = neighbour.label
            if (!startEdges.contains(neighbour) && label == None) {
              result += neighbour
            }
          })
        })
      })
      //println(s"Neighbours: $startEdges -> $result")
      result
    }
  }

  def getPunterNeighbours(punter: Punter): Iterable[Graph[Node, LUnDiEdge]#EdgeT] = {
    val punterEdges = getPunterEdges(punter).toSet
    getNeighbours(punterEdges)
  }

  def getForeignNeighbours(punter: Punter, nPunters: Int): MMap[Punter, Iterable[Graph[Node, LUnDiEdge]#EdgeT]] = {
    val foreignEdges = getForeignEdges(punter).toSet

    val g = graph
    var result = MMap.empty[Punter, Iterable[Graph[Node, LUnDiEdge]#EdgeT]]
    (0 to nPunters).foreach({
      punterId =>
        if (punterId != punter.id) {
          val foreigner = Punter(punterId)
          result.put(foreigner, getPunterNeighbours(foreigner))
        }
    })
    result
  }

  def getFreeNearMines(): Iterable[Graph[Node, LUnDiEdge]#EdgeT] = {
    val free = getFreeEdges
    val mines = getMineNodes.toSet

    free.filterNot {
      edge: Graph[Node, LUnDiEdge]#EdgeT => edge.nodes.toSet.intersect(mines).isEmpty
    }
  }

  def getPunterSubgraph(punter: Punter): GraphMap = {
    val g = graph
    val newGraph = g filter g.having(edge = {
      edge: g.EdgeT => (edge.label != None) && (edge.label == punter)
    })
    GraphMap(newGraph)
  }

  def punterDistance(punter: Punter, source: Node, target: Node): Int = {
    getPunterSubgraph(punter).distance(source, target)
  }

  def futuresScore(futures: Option[List[Future]], punter: Punter): Int = {
    var result = 0
    val subgraph = getPunterSubgraph(punter).graph
    if (futures.isDefined) {
      futures.get.foreach({
        future: Future =>
          val mine = Mine(future.sourceId)
          val site = Site(future.targetId)
          val node1Opt = subgraph find mine
          val node2Opt = subgraph find site
          val d = distance(mine, site)
          val value = d * d * d
          val fullfilled =
            (node1Opt, node2Opt) match {
              case (Some(node1), Some(node2)) =>
                (node1 pathTo node2) match {
                  case None => false
                  case _ => true
                }
              case _ => false
            }
          if (fullfilled) {
            result += value
          } else {
            result -= value
          }
      })
    }
    result
  }

  def scoreMineSite(punter: Punter, subgraph: GraphMap, mine: Node, site: Node): Int = {
    if (subgraph.hasPath(mine, site)) {
      val d = distance(mine, site)
      d * d
    } else {
      0
    }
  }

  def scoreMine(punter: Punter, subgraph: GraphMap, mine: Node): Int = {
    val g = graph
    g.nodes.toSeq.map({
      node: Graph[Node, LUnDiEdge]#NodeT => scoreMineSite(punter, subgraph, mine, node.value)
    }).sum
  }

  def score(punter: Punter, futures: Option[List[Future]]): Int = {
    val subgraph = getPunterSubgraph(punter)
    val commonScore = getMineNodes.toSeq.map({
      node: Graph[Node, LUnDiEdge]#NodeT => scoreMine(punter, subgraph, node.value)
    }).sum
    commonScore + futuresScore(futures, punter)
  }

  def test(): Unit = {
    val g = graph
    for (c <- g.componentTraverser()) {
      log.debug(c.nodes)
    }
  }
}

object GraphMap {
  def fromMap(map: Map): GraphMap = {
    GraphMap(map.toGraph())
  }
}
