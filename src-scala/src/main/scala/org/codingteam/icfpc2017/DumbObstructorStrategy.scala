package org.codingteam.icfpc2017

import org.codingteam.icfpc2017.Messages.{Move, Pass}

import scala.util.Random

/**
  * Created by portnov on 8/5/17.
  */
class DumbObstructorStrategy extends Strategy {

  private var rng = Random

  override def nextMove(): Move = {
    var candidates = graph.getFreeNearMines()
    if (candidates.isEmpty) {
      candidates = graph.getForeignNeighbours(me)
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

  override def updateState(moves: Seq[Move]): Unit = {}

  override def goodMoveProbability(): Double = {
    1
  }

}
