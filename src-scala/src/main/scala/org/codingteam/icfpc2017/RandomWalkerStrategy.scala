package org.codingteam.icfpc2017

import org.codingteam.icfpc2017.Common.Punter
import org.codingteam.icfpc2017.Messages.{Claim, Move}
import org.json4s.JsonAST.{JNothing, JValue}
import scala.util.Random

/**
  * Created by minoru on 8/5/17.
  */
class RandomWalkerStrategy extends Strategy {

  private var graph: GraphMap = GraphMap.fromMap(GameMap.Map.createEmpty)

  private var rng = Random

  override def map_=(map: GameMap.Map): Unit = {
    super.map = map
    graph = GraphMap.fromMap(map)
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
    for (move <- moves) {
      move match {
        case Claim(punter, source, target) => {
          val sourceNode = map.siteToNode(source)
          val targetNode = map.siteToNode(target)
          graph.removeEdge(sourceNode, targetNode)
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
