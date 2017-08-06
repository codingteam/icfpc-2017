package org.codingteam.icfpc2017.strategy

import org.codingteam.icfpc2017.GameMap.Mine
import org.codingteam.icfpc2017.Messages.{Move, Pass}
import org.codingteam.icfpc2017.futures.Future
import org.codingteam.icfpc2017.{Canceller, GameMap, Logging, Messages}

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * Created by portnov on 8/6/17.
  */
class MineOccupationStrategy extends Strategy with Logging {

  private var rng = Random

  override def nextMove(deadLineMs: Long, cancel: Canceller): Move = {
    val g = graph.graph
    val allMineNodes = g.nodes.filter {
      node: g.NodeT => node.value.isInstanceOf[Mine]
    }
    var futureMineNodes: Iterable[g.NodeT] =
      commonState.futures match {
        case None => List()
        case Some(list) =>
          list.map({
            future: Future => g get Mine(future.sourceId)
          })
      }

    var candidates: ListBuffer[g.EdgeT] = ListBuffer()

    // first, try to reach futures mines
    futureMineNodes.foreach({
      mineNode: g.NodeT => {
        cancel.checkCancelled()
        val noMyEdges = mineNode.edges.filter(_.label == me).isEmpty
        if (noMyEdges) {
          val freeEdges = mineNode.edges.filter(_.label == None)
          if (!freeEdges.isEmpty) {
            candidates += freeEdges.head
          }
        }
      }
    })

    // second, try to reach each mine at least once
    if (candidates.isEmpty) {
      allMineNodes.foreach({
        mineNode: g.NodeT => {
          val noMyEdges = mineNode.edges.filter(_.label == me).isEmpty
          if (noMyEdges) {
            val freeEdges = mineNode.edges.filter(_.label == None)
            if (!freeEdges.isEmpty) {
              candidates += freeEdges.head
            }
          }
        }
      })
    }

    // third, try to occupy all mines
    if (candidates.isEmpty) {
      allMineNodes.foreach({
        mineNode: g.NodeT => {
          val freeEdges = mineNode.edges.filter(_.label == None)
          if (!freeEdges.isEmpty) {
            candidates += freeEdges.head
          }
        }
      })
    }

    if (candidates.isEmpty) {
      log.debug("Mine occupation strategy cant find a good move.")
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
      val score = graph.score(me, commonState.futures)
      val our = graph.getPunterEdges(me).size
      val total = graph.graph.edges.size
      log.debug(s"Our expected score: $score, our edges: $our, total edges: $total")
      Messages.Claim(me, from, to)
    }
  }

  override def updateState(moves: Seq[Move]) = {}


  override def goodMoveProbability(): Double = {
    val g = graph.graph
    val allMineNodes = g.nodes.filter {
      node: g.NodeT => node.value.isInstanceOf[Mine]
    }

    var futureMineNodes: Iterable[g.NodeT] =
      commonState.futures match {
        case None => allMineNodes
        case Some(list) =>
          list.map({
            future: Future => g get Mine(future.sourceId)
          })
      }

    var futureMines = 0
    var freeMines = 0
    var underoccupiedMines = 0

    // first, try to reach futures mines
    futureMineNodes.foreach({
      mineNode: g.NodeT => {
        val noMyEdges = mineNode.edges.filter(_.label == me).isEmpty
        if (noMyEdges) {
          val freeEdges = mineNode.edges.filter(_.label == None)
          if (!freeEdges.isEmpty) {
            futureMines += 1
          }
        }
      }
    })

    // second, try to reach each mine at least once
    allMineNodes.foreach({
      mineNode: g.NodeT => {
        val noMyEdges = mineNode.edges.filter(_.label == me).isEmpty
        if (noMyEdges) {
          val freeEdges = mineNode.edges.filter(_.label == None)
          if (!freeEdges.isEmpty) {
            //log.debug(s"Free mine: $mineNode, edges: ${mineNode.edges}")
            freeMines += 1
          }
        }
      }
    })

    // third, try to occupy all mines
    if (freeMines == 0) {
      allMineNodes.foreach({
        mineNode: g.NodeT => {
          val freeEdges = mineNode.edges.filter(_.label == None)
          if (!freeEdges.isEmpty) {
            underoccupiedMines += 1
          }
        }
      })
    }

    if (futureMines > 0) {
      log.debug(s"There are mines mentioned in our futures, but untouched by us: $futureMines.")
      10.0
    } else if (freeMines > 0) {
      log.debug(s"There are mines untouched by us: $freeMines.")
      5.0
    } else if (underoccupiedMines > 0) {
      log.debug(s"There are mines that are not fully occupied by us yet: ${underoccupiedMines}.")
      0.5
    } else {
      log.debug("All mines are already occupied.")
      0
    }
  }

}
