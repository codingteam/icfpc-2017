package org.codingteam.icfpc2017

import org.codingteam.icfpc2017.GameMap.Node
import org.codingteam.icfpc2017.Messages.{Move, Pass}

import scala.collection.mutable.{Map => MMap, Set => MSet}
import scala.util.Random

/**
  * Created by portnov on 8/5/17.
  */
class ComponentConnectorStrategy extends Strategy {

  private var rng = Random

  def getComponents() : Seq[Iterable[Node]] = {
      val g = graph.graph
      val subgraph = g filter g.having(edge = {
        edge: g.EdgeT => (edge.label != None) && (edge.label == me)
      })
      val components =
        (for (c <- subgraph.componentTraverser())
          yield c.nodes.map(_.value)
        ).toSeq
      //println(s"Components: $components")
      components
  }

  override def nextMove(): Move = {
    var candidates = graph.getFreeNearMines()
    if (candidates.isEmpty) {
      val components = getComponents()
      val componentsNumber = components.size
      /*println(s"Found components: ${components.size}")
      for (c <- components) {
        println(c)
      }*/
      if (componentsNumber > 1) {
        val component1Idx = rng.nextInt(componentsNumber)
        val component2Idx = (component1Idx + 1) % componentsNumber
        val component1 = components(component1Idx)
        val component2 = components(component2Idx)
        var bestNodes = (component1.toIndexedSeq(0), component2.toIndexedSeq(0))
        var bestRho = 1000500
        component1.foreach({
          node1: Node => component2.foreach({
            node2: Node => {
              val d = graph.distance(node1, node2)
              if (d < bestRho) {
                bestRho = d
                bestNodes = (node1, node2)
              }
            }
          })
        })
        val g = graph.graph
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

  override def goodMoveProbability(): Double = {
    val g = graph.graph
    val subgraph = g filter g.having(edge = {
      edge: g.EdgeT => (edge.label != None) && (edge.label == me)
    })
    val componentsNumber = getComponents().size
    println(s"Found components: ${componentsNumber}")
    if (componentsNumber > 1) {
      /*for (c <- components) {
        println(c)
      }*/
      2
    } else {
      0.0
    }
  }

  override def updateState(moves: Seq[Move]): Unit = {}
}
