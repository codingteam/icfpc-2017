package org.codingteam.icfpc2017

import org.codingteam.icfpc2017.Common.Punter
import org.codingteam.icfpc2017.Messages.{Claim, Move}
import org.json4s.JsonAST.{JNothing, JValue}

import scala.util.Random

class EagerStrategy extends Strategy {

  private var graph: GraphMap = GraphMap.fromMap(GameMap.Map.createEmpty)

  private var rng = Random

  override def map_=(map: GameMap.Map): Unit = {
    super.map = map
    graph = GraphMap.fromMap(map)
  }

  override def nextMove(): Move = {
    println("Graph: " + graph.graph.toString())
    var neighbours = graph.getPunterNeighbours(me)
    if (neighbours.isEmpty) {
      neighbours = graph.getFreeEdges()
    }
    if (neighbours.isEmpty) {
      Messages.Pass(me)
    } else {
      var best = neighbours.toIndexedSeq(0)
      var score = 0
      for (edge <- neighbours) {
        var hypothesis = graph.copy()
        hypothesis.mark(edge._1.value, edge._2.value, me)
        val newScore = hypothesis.score(me)
        println("Adding " + edge.toString() + " will give us a score of " + newScore + " (" + (newScore-score) + ")")
        if (newScore > score) {
          score = newScore
          best = edge
        }
      }

      val from = best._1.value match {
        case x@GameMap.Site(id) => x
        case GameMap.Mine(id) => GameMap.Site(id)
      }
      val to = best._2.value match {
        case x@GameMap.Site(id) => x
        case GameMap.Mine(id) => GameMap.Site(id)
      }

      Messages.Claim(me, from, to)
    }
  }

  override def updateState(moves: Seq[Move]) = {
    for (move <- moves) {
      move match {
        case Claim(punter, source, target) => {
          val sourceNode = map.siteToNode(source)
          val targetNode = map.siteToNode(target)
          if (punter == me) {
            graph.mark(sourceNode, targetNode, me)
          } else {
            graph.removeEdge(sourceNode, targetNode)
          }
        }
        case _ => {}
      }
    }
  }

  override def goodMoveProbability(): Double = {
    // TODO: repalce by a real probability
    1
  }

  def state: JValue = JNothing

  def state_=(s: JValue) = ()
}
