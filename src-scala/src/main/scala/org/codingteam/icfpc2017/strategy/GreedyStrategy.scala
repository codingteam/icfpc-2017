package org.codingteam.icfpc2017.strategy

import java.io.{DataInputStream, DataOutputStream, InputStream, OutputStream}

import org.codingteam.icfpc2017.Messages.{Claim, Move}
import org.codingteam.icfpc2017.{Canceller, CommonState, GameMap, GraphMap, Logging, Messages, SerializationUtils}

import scala.util.Random
import scalax.collection.mutable.Graph

class GreedyStrategy extends Strategy with Logging {

  private var _graph: GraphMap = GraphMap.fromMap(GameMap.Map.createEmpty)

  override def graph: GraphMap = _graph

  override def commonState_=(s: CommonState): Unit = {
    super.commonState_=(s)
    _graph = GraphMap.fromMap(s.map)
  }

  private var rng = Random

  override def nextMove(deadLineMs: Long, cancel: Canceller): Move = {
    var neighbours = graph.getPunterNeighbours(me)
    if (neighbours.isEmpty) {
      neighbours = graph.getFreeNearMines()
    }
    if (neighbours.isEmpty) {
      neighbours = graph.getFreeEdges()
    }
    if (neighbours.isEmpty) {
      Messages.Pass(me)
    } else {
      var best = neighbours.toIndexedSeq(0)
      var score = 0
      for (edge <- neighbours) {
        cancel.checkCancelled()
        var hypothesis = GraphMap(Graph.from(graph.graph.nodes, graph.graph.edges))
        hypothesis.setDisatanceCache(graph.getDistanceCacheCopy())
        hypothesis.mark(edge._1.value, edge._2.value, me)
        val newScore = hypothesis.score(me, commonState.futures)
        if (newScore > score) {
          score = newScore
          best = edge
        }
        graph.setDisatanceCache(hypothesis.getDistanceCacheCopy())
      }

      val from = best._1.value match {
        case x@GameMap.Site(id) => x
        case GameMap.Mine(id) => GameMap.Site(id)
      }
      val to = best._2.value match {
        case x@GameMap.Site(id) => x
        case GameMap.Mine(id) => GameMap.Site(id)
      }

      //log.debug(s"Our expected score: $score")
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

  override def read(is: InputStream): Unit = {
    val data = new DataInputStream(is)
    _graph = SerializationUtils.readGraph(data)
  }

  override def write(os: OutputStream): Unit = {
    val data = new DataOutputStream(os)
    SerializationUtils.writeGraph(graph, data)
  }
}
