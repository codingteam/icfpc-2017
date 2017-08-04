package org.codingteam.icfpc2017

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization

import scala.io.Source

object GameMap {

  type SiteId = Int

  case class Site(id: SiteId)

  case class River(source: SiteId, target: SiteId)

  implicit val formats = Serialization.formats(NoTypeHints)
    // Serialization.formats(FullTypeHints(List(classOf[Map], classOf[Site], classOf[River])))
  // }

  object Map {
    def fromJson(str : String) : Map = {
      return parse(str).extract[Map]
    }

    def fromJsonFile(path : String) : Map = {
      val source = Source.fromFile(path)
      return parse(source.reader()).extract[Map]
    }
  }

  case class Map(sites: List[Site], rivers: List[River], mines: List[SiteId]) {
    def toJson() : String = {
      return Serialization.write(this)
    }
  }

}
