package org.codingteam.icfpc2017.strategy

import org.codingteam.icfpc2017.Messages.{Claim, Move}
import org.codingteam.icfpc2017.{CommonState, GameMap, GraphMap, Messages}

import scala.util.Random

/**
  * Created by minoru on 8/5/17.
  */
class RandomWalkerStrategy extends Strategy {

  private var _graph: GraphMap = GraphMap.fromMap(GameMap.Map.createEmpty)

  override def graph: GraphMap = _graph

  private var rng = Random

  override def commonState_=(s: CommonState): Unit = {
    super.commonState_=(s)
    _graph = GraphMap.fromMap(s.map)
  }

  override def nextMove(): Move = {
    val freeEdges = graph.getFreeEdges()
    val index = rng.nextInt(freeEdges.size)
    val edge = freeEdges.toIndexedSeq(index)

    val from = edge._1.value match {
      case x@GameMap.Site(id) => x
      case GameMap.Mine(id) => GameMap.Site(id)
    }
    val to = edge._2.value match {
      case x@GameMap.Site(id) => x
      case GameMap.Mine(id) => GameMap.Site(id)
    }

    Messages.Claim(me, from, to)
  }

  override def updateState(moves: Seq[Move]) = {
    moves.foreach {
      case Claim(punter, source, target) => {
        val sourceNode = map.siteToNode(source)
        val targetNode = map.siteToNode(target)
        graph.removeEdge(sourceNode, targetNode)
      }
      case _ =>
    }
  }

  override def goodMoveProbability(): Double = {
    1 / graph.getFreeEdges().size
  }

}
