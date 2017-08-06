package org.codingteam.icfpc2017

import java.io.{DataInputStream, DataOutputStream}

import org.codingteam.icfpc2017.Common.Punter
import org.codingteam.icfpc2017.Messages.Settings
import org.codingteam.icfpc2017.GameMap.{Mine, Node, River, Site, SiteId}
import org.codingteam.icfpc2017.futures.Future

import scala.collection.mutable
import scalax.collection.edge.LUnDiEdge
import scalax.collection.mutable.Graph


/**
  * Utilities for serialization.
  */
object SerializationUtils {
  def writeMagic(magic: String, os: DataOutputStream): Unit = {
    os.write(magic.getBytes)
  }

  def checkMagic(magic: String, is: DataInputStream): Unit = {
    val correctMagic = magic.getBytes.forall(_ == is.read().toByte)
    if (!correctMagic) {
      throw new IllegalArgumentException("Wrong magic value, expected " + magic)
    }
  }

  def writeMap(map: GameMap.Map, os: DataOutputStream): Unit = {
    // assert that SiteId is Int.
    os.writeInt(map.sites.size)
    map.sites foreach (s => os.writeInt(s.id.toInt))
    os.writeInt(map.mines.size)
    map.mines foreach (m => os.writeInt(m.toInt))
    //writeMagic("RI", os)
    os.writeInt(map.rivers.size)
    map.rivers foreach { r =>
      os.writeInt(r.source.toInt)
      os.writeInt(r.target.toInt)
    }
  }

  def writeSettings(settings: Option[Settings], os: DataOutputStream): Unit = {
    val present = settings.isDefined
    os.writeBoolean(present)
    if (present) {
      os.writeBoolean(settings.get.futures)
    }
  }

  def readSettings(is: DataInputStream): Option[Settings] = {
    val present = is.readBoolean()
    if (present) {
      val futures = is.readBoolean()
      Some(Settings(futures))
    } else {
      None
    }
  }

  def writeFutures(futures: Option[List[Future]], os: DataOutputStream): Unit = {
    futures match {
      case None => os.writeInt(-1)
      case Some(list) =>
        os.writeInt(list.size)
        list.foreach({
          future: Future => {
            os.writeInt(future.sourceId.toInt)
            os.writeInt(future.targetId.toInt)
          }
        })
    }
  }

  def readFutures(is: DataInputStream): Option[List[Future]] = {
    val size = is.readInt()
    if (size == -1) {
      None
    } else {
      val futures =
        (1 to size).map({
          i: Int => {
            val source = BigInt(is.readInt())
            val target = BigInt(is.readInt())
            Future(source, target)
          }
        }).toList
      Some(futures)
    }
  }

  def readMap(is: DataInputStream): GameMap.Map = {
    val sitesCount = is.readInt()
    val sites = Array.ofDim[Site](sitesCount)
    for (i <- sites.indices)
      sites(i) = Site(is.readInt())

    val minesCount = is.readInt()
    val mines = Array.ofDim[SiteId](minesCount)
    for (i <- mines.indices)
      mines(i) = is.readInt()

    //checkMagic("RI", is)
    val riversCount = is.readInt()
    val rivers = Array.ofDim[River](riversCount)
    for (i <- rivers.indices) {
      rivers(i) = River(is.readInt(), is.readInt())
    }
    new GameMap.Map(sites, rivers, mines)
  }

  def writeGraph(map: GraphMap, os: DataOutputStream): Unit = {
    val graph = map.graph

    val nodes = graph.nodes.toIndexedSeq
    os.writeInt(nodes.size)
    nodes foreach { node =>
      node.value match {
        case Site(id) =>
          os.writeByte(1)
          os.writeInt(id.toInt)
        case Mine(id) =>
          os.writeByte(2)
          os.writeInt(id.toInt)
      }
    }

    val edges = graph.edges.toIndexedSeq
    os.writeInt(edges.size)
    edges foreach { edge =>
      val ns = edge.nodes.toIndexedSeq
      assert(ns.size == 2)
      os.writeInt(ns(0).value.id.toInt)
      os.writeInt(ns(1).value.id.toInt)
      val punterId =
        if (edge.label == None) {
          -1
        } else {
          edge.label.asInstanceOf[Some[Punter]] map (_.id.toInt) getOrElse -1
        }
      os.writeInt(punterId)
    }
  }

  def readGraph(is: DataInputStream): GraphMap = {
    val graph = Graph[Node, LUnDiEdge]()
    val sites = mutable.HashMap[SiteId, Node]()
    val nodeCount = is.readInt()
    for (_ <- 0 until nodeCount) {
      val node = is.readByte() match {
        case 1 => Site(is.readInt())
        case 2 => Mine(is.readInt())
        case x => throw new Exception(s"Unexpected site kind: $x")
      }
      sites += node.id -> node
      graph.add(node)
    }

    val edgeCount = is.readInt()
    for (_ <- 0 until edgeCount) {
      val n1 = sites(is.readInt())
      val n2 = sites(is.readInt())
      val punter = is.readInt() match {
        case -1 => None
        case id => Some(Punter(id))
      }
      val edge = LUnDiEdge(n1, n2)(punter)
      graph.add(edge)
    }

    new GraphMap(graph)
  }
}
