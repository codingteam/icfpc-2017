package org.codingteam.icfpc2017.strategy

import java.io.{DataInputStream, DataOutputStream, InputStream, OutputStream}

import org.codingteam.icfpc2017.GameMap.{Mine, Site}
import org.codingteam.icfpc2017.Messages.{Claim, Move, Pass}
import org.codingteam.icfpc2017.futures.Future
import org.codingteam.icfpc2017.{Canceller, CommonState, GameMap, GraphMap, Logging, Messages, SerializationUtils}

import scala.util.Random

/**
  * Created by portnov on 8/6/17.
  */
class FutureStrategy extends Strategy with Logging {

  private var _graph: GraphMap = GraphMap.fromMap(GameMap.Map.createEmpty)

  override def graph: GraphMap = _graph

  private var rng = Random

  override def commonState_=(s: CommonState): Unit = {
    super.commonState_=(s)
    _graph = GraphMap.fromMap(s.map)
  }

  override def nextMove(deadLineMs: Long, cancel: Canceller): Move = {
    if (!commonState.futures.isDefined) {
      return Pass(me)
    } else {
      val g = graph.graph
      val freeSubgraph = g filter g.having(edge = {
        edge: g.EdgeT => edge.label == None
      })
      val moves = commonState.futures.get.flatMap {
        future: Future =>
          cancel.checkCancelled()
          val mineNode = g get (Mine(future.sourceId))
          // FIXME
          val ourNodes = mineNode.withSubgraph(edges = _.label == me)
          val ourSites = (for {node <- ourNodes} yield node.value).toSet
          if (ourSites.contains(Site(future.targetId)) && ourSites.contains(mineNode)) {
            //println(s"Future is already fullfilled: $future.")
            None
          } else {
            val targetSite = Site(future.targetId)
            //val targetNode = g get targetSite
            (freeSubgraph find targetSite) match {
              case None =>
                //println(s"Future can not be fullfilled: $future. All approaches to it are claimed by other punters.")
                None
              case Some(targetNode) => {

                var bestPath: Option[freeSubgraph.Path] = None
                var bestRho = 1000500

                ourNodes.foreach({
                  gNode: g.NodeT =>
                    freeSubgraph find (gNode.value) match {
                      case None =>
                        //println(s"Future can not be fullfilled: $future. All firtst steps to it are claimed by other punters.")
                        None
                      case Some(node) =>
                        (node shortestPathTo targetNode) match {
                          case None =>
                          case Some(path) => {
                            if (path.length < bestRho) {
                              bestRho = path.length
                              bestPath = Some(path)
                            }
                          }
                        }
                    }
                })

                bestPath match {
                  case None => {
                    //println(s"Future can not be fullfilled: $future. Threre is no free way.")
                    None
                  }
                  case Some(path) => {
                    val edge = path.edges.head
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
                    log.info(s"Our expected score: $score, our edges: $our, total edges: $total")*/
                    Some(Messages.Claim(me, from, to))
                  }
                }
              }
            }
          }
      }

      moves.headOption match {
        case None =>
          log.debug("Can not fullfill any future")
          Pass(me)
        case Some(move) => move
      }
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
    val (fullfilled, total) = commonState.getFutureStats()
    if (fullfilled < total) 0.95 else 0.0
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
