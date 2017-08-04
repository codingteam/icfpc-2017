package org.codingteam.icfpc2017

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

import scala.io.Source

object GameMap {

  type SiteId = BigInt

  case class Site(id: SiteId)

  case class River(source: SiteId, target: SiteId) {
    def toEdge(map : Map) : UnDiEdge[Site] = {
      return UnDiEdge(map.siteMap.get(source).get, map.siteMap.get(target).get)
    }
  }

  implicit val formats = Serialization.formats(NoTypeHints)

  object Map {
    def fromJson(str: String): Map = {
      return parse(str).extract[Map]
    }

    def fromJsonFile(path: String): Map = {
      val source = Source.fromFile(path)
      return parse(source.reader()).extract[Map]
    }
  }

  class Map(var sites: List[Site], var rivers: List[River], var mines: List[SiteId]) {

    var siteMap = sites.map(site => (site.id, site)).toMap

    def toJson(): String = {
      return Serialization.write(this)
    }

    def toGraph(): Graph[Site, UnDiEdge] = {
      val edges = rivers.map(r => r.toEdge(this))
      return Graph.from(sites, edges)
    }
  }

}
