package org.codingteam.icfpc2017.futures

import java.io.{DataInputStream, DataOutputStream, InputStream, OutputStream}

import org.codingteam.icfpc2017.GameMap.{Node, Mine, Map}
import org.codingteam.icfpc2017.Messages.{Move, Pass}
import org.codingteam.icfpc2017.{GameMap, Messages, GraphMap}

import scala.collection.mutable.{Map => MMap, Set => MSet}
import scala.util.Random
import scalax.collection.edge.LUnDiEdge
import scalax.collection.mutable.Graph
import scalax.collection.edge.LBase.LEdgeImplicits

case class RandomFutureGenerator(val map: Map, val maxDistance: Int) {
  private var rng = Random

  def generate(): List[Future] = {
    val g = map.toGraph
    val mineNodes = g.nodes.filter {
      node: Graph[Node, LUnDiEdge]#NodeT => node.value.isInstanceOf[Mine]
    }
    mineNodes.flatMap({
      mineNode: g.NodeT => {
        //val availableNodes = mineNode.withSubgraph(nodes = ! _.value.isInstanceOf[Mine]).withMaxDepth(maxDistance)
        val availableNodes = mineNode.withMaxDepth(maxDistance)
        //println(s"Mine ${mineNode.value} available nodes: ${availableNodes.toList}")
        val availableSites = availableNodes.filterNot(_.value.isInstanceOf[Mine])
        if (availableSites.isEmpty) {
          None
        } else {
          val index = rng.nextInt(availableSites.size)
          val site = availableSites.toIndexedSeq(index)
          Some(Future(mineNode.value.id, site.id))
        }
      }
    }).toList
  }
}

