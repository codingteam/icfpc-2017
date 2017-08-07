package org.codingteam.icfpc2017.strategy

import org.codingteam.icfpc2017.Messages.{Move, Pass}
import org.codingteam.icfpc2017.Common.Punter
import org.codingteam.icfpc2017.{Canceller, GameMap, Logging, Messages}
import org.codingteam.icfpc2017.GameMap.Node

import scala.collection.mutable.ListBuffer
import scala.util.Random
import scalax.collection.edge.LUnDiEdge
import scalax.collection.mutable.Graph
import scalax.collection.edge.LBase.LEdgeImplicits

/**
  * Created by portnov on 8/5/17.
  */
class DumbObstructorStrategy extends Strategy with Logging {

  private var rng = Random

  override def nextMove(deadLineMs: Long, cancel: Canceller): Move = {
    val allCandidates = graph.getForeignNeighbours(me, commonState.punterCount)
    log.debug(s"All: $allCandidates")
    val bestPunter = getBestPunter()._1
    val candidates: Iterable[Graph[Node, LUnDiEdge]#EdgeT] = allCandidates.get(Punter(bestPunter)).getOrElse(List())

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
      /*val score = graph.score(me, commonState.futures)
      val our = graph.getPunterEdges(me).size
      val total = graph.graph.edges.size
      log.debug(s"Our expected score: $score, our edges: $our, total edges: $total")*/
      Messages.Claim(me, from, to)
    }
  }

  override def updateState(moves: Seq[Move]): Unit = {}

  def getBestPunter() : (Int, Int) = {
    var bestPunterId : Int = 0
    var bestScore = 0

    (0 to commonState.punterCount).foreach({
      punterId =>
        if (punterId != me.id) {
          val score = graph.score(Punter(punterId), None)
          //log.debug(s"Punter #$punterId got $score")
          if (score > bestScore) {
            bestPunterId = punterId
            bestScore = score
          }
        }
    })

    log.debug(s"Best of our competitors is #${bestPunterId}, he got $bestScore.")
    (bestPunterId, bestScore)
  }

  override def goodMoveProbability(): Double = {
    // TODO: move probability.
    0.3
  }

}
