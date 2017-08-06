package org.codingteam.icfpc2017.strategy

import org.codingteam.icfpc2017.GameMap.Mine
import org.codingteam.icfpc2017.Messages.{Move, Pass}
import org.codingteam.icfpc2017.{GameMap, Messages}

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * Created by portnov on 8/6/17.
  */
class MineOccupationStrategy extends Strategy {

  private var rng = Random

  override def nextMove(): Move = {
    val g = graph.graph
    val mineNodes = g.nodes.filter {
      node: g.NodeT => node.value.isInstanceOf[Mine]
    }
    var candidates : ListBuffer[g.EdgeT] = ListBuffer()
    //var candidate : Option[g.EdgeT] = None

    // first, try to reach each mine at least once
    mineNodes.foreach({
      mineNode : g.NodeT => {
        val noMyEdges = mineNode.edges.filter(_.label == me).isEmpty
        if (noMyEdges) {
          val freeEdges = mineNode.edges.filter(_.label == None)
          if (! freeEdges.isEmpty) {
            candidates += freeEdges.head
          }
        }
      }
    })

    // second, try to occupy all mines
    if (candidates.isEmpty) {
      mineNodes.foreach({
        mineNode : g.NodeT => {
          val freeEdges = mineNode.edges.filter(_.label == None)
          if (! freeEdges.isEmpty) {
            candidates += freeEdges.head
          }
        }
      })
    }

    if (candidates.isEmpty) {
      println("Mine occupation strategy cant find a good move.")
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

  override def updateState(moves: Seq[Move]) = {}


  override def goodMoveProbability(): Double = {
    val g = graph.graph
    val mineNodes = g.nodes.filter {
      node: g.NodeT => node.value.isInstanceOf[Mine]
    }

    var foundFreeMine = false
    var foundUnderoccupiedMine = false
    // first, try to reach each mine at least once
    mineNodes.foreach({
      mineNode : g.NodeT => {
        val noMyEdges = mineNode.edges.filter(_.label == me).isEmpty
        if (noMyEdges) {
          val freeEdges = mineNode.edges.filter(_.label == None)
          if (! freeEdges.isEmpty) {
            foundFreeMine = true
          }
        }
      }
    })

    // second, try to occupy all mines
    if (! foundFreeMine) {
      mineNodes.foreach({
        mineNode : g.NodeT => {
          val freeEdges = mineNode.edges.filter(_.label == None)
          if (! freeEdges.isEmpty) {
            foundUnderoccupiedMine = true
          }
        }
      })
    }

    if (foundFreeMine) {
      println("There is a totally free mine.")
      5.0
    } else if (foundUnderoccupiedMine) {
      println("There is a mine that is not occupied by us yet.")
      0.5
    } else {
      println("All mines are already occupied.")
      0
    }
  }

}
