package org.codingteam.icfpc2017

import org.codingteam.icfpc2017.Common.Punter
import org.codingteam.icfpc2017.GameMap.Node
import org.codingteam.icfpc2017.Messages.{Claim, Pass, Move}
import org.json4s.JsonAST.{JNothing, JValue}
import scala.util.Random
import scala.collection.mutable.{Map => MMap, Set => MSet}
import scalax.collection.edge.LUnDiEdge
import scalax.collection.mutable.Graph
import scalax.collection.edge.LBase.LEdgeImplicits

/**
  * Created by portnov on 8/5/17.
  */
class ComponentConnectorStrategy extends Strategy {

  private var graph: GraphMap = GraphMap.fromMap(GameMap.Map.createEmpty)

  private var rng = Random

  override def map_=(map: GameMap.Map): Unit = {
    super.map = map
    graph = GraphMap.fromMap(map)
  }

  override def nextMove(): Move = {
    var candidates = graph.getFreeNearMines()
    if (candidates.isEmpty) {
      val g = graph.graph
      val subgraph = g filter g.having(edge = {
        edge: g.EdgeT => (edge.label != None) && (edge.label == me)
      })
      val components = (
        for (c <- subgraph.componentTraverser())
          yield c.nodes
      ).toSeq
      println(s"Found components: ${components.size}")
      for (c <- components) {
        println(c)
      }
      if (components.size > 1) {
        val component1Idx = rng.nextInt(components.size)
        val component2Idx = (component1Idx + 1) % components.size
        val component1 = components(component1Idx)
        val component2 = components(component2Idx)
        var bestNodes = (component1.toIndexedSeq(0).value, component2.toIndexedSeq(0).value)
        var bestRho = 1000500
        component1.foreach({
          node1: Graph[Node, LUnDiEdge]#NodeT => component2.foreach({
            node2: Graph[Node, LUnDiEdge]#NodeT => {
              val d = graph.distance(node1.value, node2.value)
              if (d < bestRho) {
                bestRho = d
                bestNodes = (node1.value, node2.value)
              }
            }
          })
        })
        val node1 = g get bestNodes._1
        val node2 = g get bestNodes._2
        (node1 shortestPathTo node2) match {
          case None =>
          case Some(path) => {
            println(s"Found path: $node1 - $node2 :: $path")
            //val edge1 = (g find LUnDiEdge(node1.value, path.nodes.head.value)(me)).get
            //val edge2 = (g find LUnDiEdge(path.nodes.last.value, node2.value)(me)).get
            candidates = List(path.edges.head, path.edges.last)
            //candidates = List(edge1, edge2)
          }
        }
      }
    }

    if (candidates.isEmpty) {
      Pass(me)
    } else {
      val index = rng.nextInt(candidates.size)
      val edge = candidates.toIndexedSeq(index)

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
      val score = graph.score(me)
      val our = graph.getPunterEdges(me).size
      val total = graph.graph.edges.size
      println(s"Our expected score: $score, our edges: $our, total edges: $total")
      Messages.Claim(me, from, to)
    }
  }

  override def updateState(moves: Seq[Move]) = {
    for (move <- moves) {
      move match {
        case Claim(punter, source, target) => {
          val sourceNode = map.siteToNode(source)
          val targetNode = map.siteToNode(target)
          graph.mark(sourceNode, targetNode, punter)
        }
        case _ => {}
      }
    }
  }


  override def goodMoveProbability(): Double = {
    1
  }

  def state: JValue = JNothing

  def state_=(s: JValue) = ()

}
