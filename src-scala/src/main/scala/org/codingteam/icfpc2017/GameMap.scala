package org.codingteam.icfpc2017

import org.codingteam.icfpc2017.Common.Punter

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization

import scala.io.Source
import scalax.collection.Graph
import scalax.collection.GraphEdge._
import scalax.collection.edge.LUnDiEdge
import scalax.collection.edge.LBase.LEdgeImplicits

object GameMap {

  type SiteId = BigInt

  abstract class Node
  case class Site(id: SiteId) extends Node
  case class Mine(id: SiteId) extends Node

  object SiteImplicit extends LEdgeImplicits[Option[Punter]]
  import SiteImplicit._

  case class River(source: SiteId, target: SiteId) {
    def toEdge(map: Map): LUnDiEdge[Node] = {
      return LUnDiEdge(map.siteMap.get(source).get, map.siteMap.get(target).get)(None)
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

    var siteMap = sites.map(site => (site.id, siteToNode(site))).toMap

    def siteToNode(site : Site) : Node = {
      if (mines.contains(site.id)) {
        Mine(site.id)
      } else {
        site
      }
    }

    def toJson(): String = {
      return Serialization.write(this)
    }

    def toGraph(): Graph[Node, LUnDiEdge] = {
      val edges = rivers.map(r => r.toEdge(this))
      return Graph.from(siteMap.values, edges)
    }

    override def toString() : String = {
      toJson()
    }
  }

}
