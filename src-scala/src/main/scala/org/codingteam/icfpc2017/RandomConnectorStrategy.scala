package org.codingteam.icfpc2017

import org.codingteam.icfpc2017.Common.Punter
import org.codingteam.icfpc2017.Messages.{Claim, Pass, Move}
import org.json4s.JsonAST.{JNothing, JValue}
import scala.util.Random

/**
  * Created by portnov on 8/5/17.
  */
class RandomConnectorStrategy extends Strategy {

  private var graph: GraphMap = GraphMap.fromMap(GameMap.Map.createEmpty)

  private var rng = Random

  override def map_=(map: GameMap.Map): Unit = {
    super.map = map
    graph = GraphMap.fromMap(map)
  }

  override def nextMove(): Move = {
    val freeEdges = graph.getPunterNeighbours(me)
    if (freeEdges.isEmpty) {
      Pass(me)
    } else {
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

      val sourceNode = map.siteToNode(from)
      val targetNode = map.siteToNode(to)
      graph.mark(sourceNode, targetNode, me)
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
