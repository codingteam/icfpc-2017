package org.codingteam.icfpc2017

import java.io.{DataInputStream, DataOutputStream}

import org.codingteam.icfpc2017.GameMap.{River, Site, SiteId}

/**
  * Utilities for serialization.
  */
object SerializationUtils {
  def writeMap(map: GameMap.Map, os: DataOutputStream): Unit = {
    // assert that SiteId is Int.
    os.writeInt(map.sites.size)
    map.sites foreach (s => os.writeInt(s.id.toInt))
    os.writeInt(map.mines.size)
    map.mines foreach (m => os.writeInt(m.toInt))
    os.writeInt(map.rivers.size)
    map.rivers foreach { r =>
      os.writeInt(r.source.toInt)
      os.writeInt(r.target.toInt)
    }
  }

  def readMap(is: DataInputStream): GameMap.Map = {
    val sitesCount = is.readInt()
    val sites = Array.ofDim[Site](sitesCount)
    for (i <- sites.indices)
      sites(i) = Site(is.readInt())

    val minesCount = is.readInt()
    val mines = Array.ofDim[SiteId](sitesCount)
    for (i <- mines.indices)
      mines(i) = is.readInt()

    val riversCount = is.readInt()
    val rivers = Array.ofDim[River](sitesCount)
    for (i <- rivers.indices) {
      rivers(i) = River(is.readInt(), is.readInt())
    }
    new GameMap.Map(sites, rivers, mines)
  }

  def writeGraph(map: GraphMap, os: DataOutputStream): Unit = {
    val graph = map.graph

  }

  def readGraph(map: GraphMap, is: DataInputStream): Unit = {
    val graph = map.graph
    graph.clear()

  }
}
