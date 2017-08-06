package org.codingteam.icfpc2017.futures

import org.codingteam.icfpc2017.GameMap.SiteId
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._

case class Future(sourceId: SiteId, targetId: SiteId) {
  def toJson(): JValue = {
    JObject("source" -> sourceId, "target" -> targetId)
  }
}
