package org.codingteam.icfpc2017

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization

import scala.io.Source
import scalax.collection.Graph
import scalax.collection.GraphEdge._

object GameMap {

  type SiteId = BigInt

  case class Site(id: SiteId)

  case class River(source: SiteId, target: SiteId) {
    def toEdge(map: Map): UnDiEdge[Site] = {
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

    def createEmpty = new Map(IndexedSeq(), IndexedSeq(), IndexedSeq())
  }

  class Map(var sites: IndexedSeq[Site],
            var rivers: IndexedSeq[River],
            var mines: IndexedSeq[SiteId]) {

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
