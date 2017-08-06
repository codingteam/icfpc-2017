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

    var freeMines = 0
    var underoccupiedMines = 0
    // first, try to reach each mine at least once
    mineNodes.foreach({
      mineNode : g.NodeT => {
        val noMyEdges = mineNode.edges.filter(_.label == me).isEmpty
        if (noMyEdges) {
          val freeEdges = mineNode.edges.filter(_.label == None)
          if (! freeEdges.isEmpty) {
            //println(s"Free mine: $mineNode, edges: ${mineNode.edges}")
            freeMines += 1
          }
        }
      }
    })

    // second, try to occupy all mines
    if (freeMines == 0) {
      mineNodes.foreach({
        mineNode : g.NodeT => {
          val freeEdges = mineNode.edges.filter(_.label == None)
          if (! freeEdges.isEmpty) {
            underoccupiedMines += 1
          }
        }
      })
    }

    if (freeMines > 0) {
      println(s"There are totally free mines: $freeMines.")
      5.0
    } else if (underoccupiedMines > 0) {
      println(s"There are mines that are not fully occupied by us yet: ${underoccupiedMines}.")
      0.5
    } else {
      println("All mines are already occupied.")
      0
    }
  }

}
