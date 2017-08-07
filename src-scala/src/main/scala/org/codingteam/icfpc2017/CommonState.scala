package org.codingteam.icfpc2017

import java.io.{DataInputStream, DataOutputStream, InputStream, OutputStream}

import org.codingteam.icfpc2017.Common.Punter
import org.codingteam.icfpc2017.GameMap.Site
import org.codingteam.icfpc2017.Messages.{Claim, Move, Pass, Settings, Splurge, AnOption}
import org.codingteam.icfpc2017.futures.Future

/**
  * Common state of all strategies.
  */
class CommonState extends Logging {

  var me: Punter = Punter(0)
  var punterCount: Int = 1
  var map: GameMap.Map = GameMap.Map.createEmpty
  var graph: GraphMap = GraphMap.fromMap(map)
  var settings: Option[Settings] = Some(Settings(false, false))
  var futures: Option[List[Future]] = None
  var lastPassCount: Int = 0

  def init(map: GameMap.Map, punterId: BigInt, punterCount: Int, settings: Option[Settings], futures: Option[List[Future]]): Unit = {
    this.map = map
    graph = GraphMap fromMap map
    me = Punter(punterId)
    this.punterCount = punterCount
    this.settings = settings
    this.futures = futures
  }

  def updateState(moves: Seq[Move]): Unit = {
    moves.foreach {
      case Claim(punter, source, target) =>
        val sourceNode = map.siteToNode(source)
        val targetNode = map.siteToNode(target)
        graph.mark(sourceNode, targetNode, punter)

      case Splurge(punter, sites) =>
        //println("!!!Splurge!")
        val pairs = sites.zip(sites.tail)
        pairs.foreach({
          case (source, target) =>
            val sourceNode = map.siteToNode(Site(source))
            val targetNode = map.siteToNode(Site(target))
            graph.mark(sourceNode, targetNode, punter)
          //println(s"Splurge $sourceNode $targetNode")
        })
      case AnOption(punter, source, target) =>
        log.error(s"We do not process Option $punter $source $target.")
      case _ =>
    }
    for (m <- moves if m.punter == me) {
      m match {
        case _: Pass => lastPassCount += 1
        case _: Claim => lastPassCount = 0
        case _: AnOption => lastPassCount = 0
        case _: Splurge => lastPassCount = 0
      }
    }
  }

  def getFullfilledFutures(): Iterable[Future] = {
    // var result: Seq[Future] = Seq()
    val subgraph = graph.getPunterSubgraph(me)
    if (futures.isDefined) {
      for {future <- futures.get
           if (subgraph.hasPath(map.siteToNode(Site(future.sourceId)), map.siteToNode(Site(future.targetId))))
      } yield future
    } else {
      List()
    }
  }

  def getFutureStats(): (Int, Int) = {
    futures match {
      case None => (0, 0)
      case Some(list) => {
        val fullfilled = getFullfilledFutures().size
        val total = list.size
        (fullfilled, total)
      }
    }
  }

  def read(is: InputStream): Unit = {
    val data = new DataInputStream(is)
    me = Punter(data.readLong())
    punterCount = data.readInt()
    lastPassCount = data.readInt()
    //SerializationUtils.checkMagic("FU", data)
    futures = SerializationUtils.readFutures(data)
    //SerializationUtils.checkMagic("SET", data)
    settings = SerializationUtils.readSettings(data)
    //SerializationUtils.checkMagic("MAP", data)
    map = SerializationUtils.readMap(data)
    //SerializationUtils.checkMagic("GR", data)
    graph = SerializationUtils.readGraph(data)
  }

  def write(os: OutputStream): Unit = {
    val data = new DataOutputStream(os)
    data.writeLong(me.id.toLong)
    data.writeInt(punterCount)
    data.writeInt(lastPassCount)
    //SerializationUtils.writeMagic("FU", data)
    SerializationUtils.writeFutures(futures, data)
    //SerializationUtils.writeMagic("SET", data)
    SerializationUtils.writeSettings(settings, data)
    //SerializationUtils.writeMagic("MAP", data)
    SerializationUtils.writeMap(map, data)
    //SerializationUtils.writeMagic("GR", data)
    SerializationUtils.writeGraph(graph, data)
  }
}

object CommonState {
  def apply(map: GameMap.Map, punterId: BigInt, punterCount: Int, settings: Option[Settings], futures: Option[List[Future]]): CommonState = {
    val r = new CommonState
    r.init(map, punterId, punterCount, settings, futures)
    r
  }
}
