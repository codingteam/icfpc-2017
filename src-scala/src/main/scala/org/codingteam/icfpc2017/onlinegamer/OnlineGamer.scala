package org.codingteam.icfpc2017.onlinegamer

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorSystem, Props}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model._
import org.codingteam.icfpc2017.{LogbackLogger, Strategy}
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer


case class StatsMapPlayers(waiting: Int, max: Int)
case class StatsMap(players: StatsMapPlayers, port: Int, mapName: String)

object MainActor {

  def props(maps: List[(StatsMap, Strategy)], name: String) = Props(new MainActor(maps, name))

  case class LoopStop(port: Int)

}

class MainActor(maps: List[(StatsMap, Strategy)], name: String) extends Actor with LogbackLogger {

  import MainActor._

  val runningLoops = ListBuffer.empty[Int]

  override def receive: Receive = {
    case "start" =>
      logger.info(s"Running for maps: ${maps}")

      if(maps.isEmpty) {
        context.system.terminate()
      } else {
        maps.foreach {
          case (map, strategy) =>
            val loopActor = context.actorOf(RunLoopActor.props(OnlineGamer.SERVER_HOST, map.port, name, strategy))

            runningLoops += map.port

            logger.info(s"Running loop: ${map.port}; $runningLoops")

            loopActor ! "loop"
        }
      }

    case LoopStop(port) =>
      runningLoops -= port

      logger.info(s"Loop quit: $port; $runningLoops")

      if(runningLoops.isEmpty) {
        context.system.terminate()
      }
  }

}


trait OnlineGamer {

  val actorSystem = ActorSystem("online-gamer")

  def runOnSpecificMaps(maps: List[String], statsMaps: List[StatsMap]): List[StatsMap]

  def strategy(): Strategy

  def parseStats(): List[StatsMap] = {
    val browser = JsoupBrowser()
    val doc = browser.get(OnlineGamer.STATS_PAGE)
    val trs = doc >> elementList("tbody tr")

    trs.drop(1).flatMap { tr =>
      val tds = tr.children.toList

      tds match {
        case List(players, _, _, port, map) =>
          HtmlParseHelpers.parsePlayers(players >> allText("td")).map { playerz =>
            StatsMap(playerz, (port >> allText("td")).toInt, HtmlParseHelpers.parseMapName(map >> allText("td")))
          }
        case _ =>
          None
      }
    }
  }


  def run(maps: List[String], name: String): Unit = {
    val mapz = runOnSpecificMaps(maps, parseStats())
    val mainActor = actorSystem.actorOf(MainActor.props(mapz.map(m => (m, strategy())), name), "main")

    mainActor ! "start"
  }

}

object OnlineGamer {

  val SERVER_HOST = "punter.inf.ed.ac.uk"

  val STATS_PAGE = "http://punter.inf.ed.ac.uk/status.html"

}


object HtmlParseHelpers {

  def parsePlayers(text: String): Option[StatsMapPlayers] = {
    val regex = "\\(([0-9]+)\\/([0-9]+)\\)".r

    regex.findFirstMatchIn(text).map(m => StatsMapPlayers(m.group(1).toInt, m.group(2).toInt))
  }

  def parseMapName(text: String): String = {
    val regex = "([\\w\\-]+)\\.json".r

    regex.findFirstMatchIn(text).map(_.group(1)).getOrElse("")
  }

}
