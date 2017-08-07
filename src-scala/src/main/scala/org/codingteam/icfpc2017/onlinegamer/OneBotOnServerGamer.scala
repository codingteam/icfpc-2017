package org.codingteam.icfpc2017.onlinegamer

import org.codingteam.icfpc2017.AppEntry
import org.codingteam.icfpc2017.strategy.Strategy

class OneBotOnServerGamer() extends OnlineGamer {

  override def runOnSpecificMaps(maps: List[String], statsMaps: List[StatsMap]): List[StatsMap] = {
    val byName = statsMaps.filter(sm => maps.contains(sm.mapName))

    byName.groupBy(_.mapName).map {
      case (_, v) =>
        v.sortWith {
          case (a, b) =>
            val deltaA = a.players.max - a.players.waiting
            val deltaB = b.players.max - b.players.waiting

            deltaA < deltaB
        }.head
    }.toList
  }

  override def strategy(): Strategy = AppEntry.selectStrategy("delegating")
}