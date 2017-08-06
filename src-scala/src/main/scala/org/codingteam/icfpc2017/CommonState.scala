package org.codingteam.icfpc2017

import java.io.{DataInputStream, DataOutputStream, InputStream, OutputStream}

import org.codingteam.icfpc2017.Common.Punter
import org.codingteam.icfpc2017.Messages.{Claim, Move}

/**
  * Common state of all strategies.
  */
class CommonState {

  var me: Punter = Punter(0)
  var punterCount: Int = 1
  var map: GameMap.Map = GameMap.Map.createEmpty
  var graph: GraphMap = GraphMap.fromMap(map)

  def init(map: GameMap.Map, punterId: BigInt, punterCount: Int): Unit = {
    this.map = map
    graph = GraphMap fromMap map
    me = Punter(punterId)
    this.punterCount = punterCount
  }

  def updateState(moves: Seq[Move]): Unit = {
    for (Claim(punter, source, target) <- moves) {
      val sourceNode = map.siteToNode(source)
      val targetNode = map.siteToNode(target)
      graph.mark(sourceNode, targetNode, punter)
    }
  }

  def read(is: InputStream): Unit = {
    val data = new DataInputStream(is)
    me = Punter(data.readLong())
    punterCount = data.readInt()
    map = SerializationUtils.readMap(data)
    SerializationUtils.readGraph(graph, data)
  }

  def write(os: OutputStream): Unit = {
    val data = new DataOutputStream(os)
    data.writeLong(me.id.toLong)
    data.writeInt(punterCount)
    SerializationUtils.writeMap(map, data)
    SerializationUtils.writeGraph(graph, data)
  }
}

object CommonState {
  def apply(map: GameMap.Map, punterId: BigInt, punterCount: Int): CommonState = {
    val r = new CommonState
    r.init(map, punterId, punterCount)
    r
  }
}
