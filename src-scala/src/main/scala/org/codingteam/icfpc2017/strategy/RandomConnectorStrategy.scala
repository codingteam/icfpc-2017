package org.codingteam.icfpc2017.strategy

import org.codingteam.icfpc2017.Messages.{Claim, Move, Pass}
import org.codingteam.icfpc2017.{CommonState, GameMap, GraphMap, Logging, Messages}

import scala.util.Random

/**
  * Created by portnov on 8/5/17.
  */
class RandomConnectorStrategy extends Strategy with Logging {

  private var _graph: GraphMap = GraphMap.fromMap(GameMap.Map.createEmpty)

  override def graph: GraphMap = _graph
  private var rng = Random

  override def commonState_=(s: CommonState): Unit = {
    super.commonState_=(s)
    _graph = GraphMap.fromMap(s.map)
  }

  override def nextMove(): Move = {
    val neighbours = graph.getPunterNeighbours(me)
    val candidates =
      if (neighbours.isEmpty) {
        graph.getFreeNearMines()
      } else {
        neighbours
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
      log.info(s"Our expected score: $score, our edges: $our, total edges: $total")
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

}
