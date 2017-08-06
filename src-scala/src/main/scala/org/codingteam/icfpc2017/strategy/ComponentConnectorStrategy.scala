package org.codingteam.icfpc2017.strategy

import java.io.{DataInputStream, DataOutputStream, InputStream, OutputStream}

import org.codingteam.icfpc2017.GameMap.Node
import org.codingteam.icfpc2017.Messages.{Move, Pass}
import org.codingteam.icfpc2017.{GameMap, Messages, Logging}

import scala.collection.mutable.{Map => MMap, Set => MSet}
import scala.util.Random
import scalax.collection.edge.LUnDiEdge
import scalax.collection.mutable.Graph
import scalax.collection.edge.LBase.LEdgeImplicits

/**
  * Created by portnov on 8/5/17.
  */
class ComponentConnectorStrategy extends Strategy with Logging {

  private var rng = Random

  private var noPaths : Boolean = false

  def getComponents(): Seq[(Iterable[Node], Int)] = {
    val g = graph.graph
    val subgraph = g filter g.having(edge = {
      edge: g.EdgeT => (edge.label != None) && (edge.label == me)
    })
    val components =
      (for (c <- subgraph.componentTraverser())
        yield c.nodes.map(_.value)
        ).toSeq.zipWithIndex
    //log.debug(s"Components: $components")
    components
  }

  def calcComponentsPath(freeSubgraph: Graph[Node, LUnDiEdge],
                         component1: (Iterable[Node], Int),
                         component2: (Iterable[Node], Int)): Option[Graph[Node, LUnDiEdge]#Path] = {

    var selectedPair: Option[(Node, Node)] = None
    var bestPath: Option[freeSubgraph.Path] = None
    var bestRho = 1000500

    component1._1.foreach({
      node1: Node =>
        component2._1.foreach({
          node2: Node => {
            val n1Opt = freeSubgraph find node1
            val n2Opt = freeSubgraph find node2
            (n1Opt, n2Opt) match {
              case (Some(n1), Some(n2)) =>
                (n1 shortestPathTo n2) match {
                  case None => // log.debug(s"No way: $n1 - $n2")
                  case Some(path) =>
                    if (path.length < bestRho) {
                      bestPath = Some(path)
                      bestRho = path.length
                      selectedPair = Some(node1, node2)
                      //log.debug(s"Found better pair: $node1 - $node2 with distance $bestRho")
                    }
                }
              case _ => // log.debug(s"Nodes not in free subgraph: $node1 - $node2")
            }
          }
        })
    })

    selectedPair match {
      case None => {
        log.debug(s"Can not select a pair of nodes to connect components #${component1._2} - #${component2._2}")
        None
      }
      case Some(bestNodes) =>
        val node1Opt = freeSubgraph find bestNodes._1
        val node2Opt = freeSubgraph find bestNodes._2
        (node1Opt, node2Opt) match {
          case (Some(node1), Some(node2)) =>
            bestPath match {
              case None => {
                log.debug(s"Both selected nodes $node1, $node2 belong to free subgraph, but there is no free way between them.")
                None
              }
              case Some(path) => {
                Some(path)
              }
            }
          case _ => {
            log.debug(s"No ways to connect components #${component1._2} - #${component2._2} found, this is odd.")
            None
          }
        }
    }
  }

  override def nextMove(): Move = {
    if (noPaths) {
      log.debug("Last time there were no paths between components, i think they could not appear now.")
      return Pass(me)
    }
    val g = graph.graph
    val freeSubgraph = g filter g.having(edge = {
      edge: g.EdgeT => edge.label == None
    })
    //log.debug(s"Free subgraph: $freeSubgraph")

    val components = getComponents()
    val componentsNumber = components.size
    /*log.debug(s"Found components: ${components.size}")
    for (c <- components) {
      log.debug(c)
    }*/

    var bestPath : Option[Graph[Node, LUnDiEdge]#Path] = None
    var bestRho = 1000500
    var bestComponentIdxs: (Int,Int) = (0,0)

    if (componentsNumber > 1) {
      for {component1 <- components
           component2 <- components
           if (component1._2 < component2._2)} {

        assert(!component1._1.isEmpty)
        assert(!component1._1.isEmpty)

        calcComponentsPath(freeSubgraph, component1, component2) match {
          case None =>
          case Some(path) => {
            val d = path.length
            if (d < bestRho) {
              bestRho = d
              bestPath = Some(path)
              bestComponentIdxs = (component1._2, component2._2)
            }
          }
        }
      }

      bestPath match {
        case None => {
          log.debug("Component connector can not find good move")
          noPaths = true
          Pass(me)
        }
        case Some(path) => {
          assert(!path.edges.isEmpty)

          log.debug(s"Found path between components #${bestComponentIdxs}:  ${path.nodes.head} - ${path.nodes.last} :: ${path.length}")
          if (path.length == 1) {
            log.debug("Will connect two components.")
          }

          val edge = path.edges.head
          val from = edge._1.value match {
            case x@GameMap.Site(id) => x
            case GameMap.Mine(id) => GameMap.Site(id)
          }
          val to = edge._2.value match {
            case x@GameMap.Site(id) => x
            case GameMap.Mine(id) => GameMap.Site(id)
          }

          val sourceNode = map.siteToNode(from)
          val targetNode = map.siteToNode(to)
          graph.mark(sourceNode, targetNode, me)
          val score = graph.score(me, commonState.futures)
          val our = graph.getPunterEdges(me).size
          val total = graph.graph.edges.size
          log.debug(s"Our expected score: $score, our edges: $our, total edges: $total")
          Messages.Claim(me, from, to)
        }
      }
    } else {
      log.debug("There is only one component, nothing to connect.")
      Pass(me)
    }
  }

  override def goodMoveProbability(): Double = {
    if (noPaths) {
      0.0
    } else {
      val g = graph.graph
      val subgraph = g filter g.having(edge = {
        edge: g.EdgeT => (edge.label != None) && (edge.label == me)
      })
      val componentsNumber = getComponents().size
      log.debug(s"Found components: ${componentsNumber}")
      if (componentsNumber > 1) {
        /*for (c <- components) {
          log.debug(c)
        }*/
        componentsNumber
      } else {
        0.0
      }
    }
  }

  override def updateState(moves: Seq[Move]): Unit = {}

  override def read(is: InputStream): Unit = {
    val data = new DataInputStream(is)
    noPaths = data.readBoolean()
  }

  override def write(os: OutputStream): Unit = {
    val data = new DataOutputStream(os)
    data.writeBoolean(noPaths)
  }
}
