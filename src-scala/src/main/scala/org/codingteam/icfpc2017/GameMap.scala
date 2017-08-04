package org.codingteam.icfpc2017

object GameMap {

  type SiteId = Int

  case class Site(id: SiteId)

  case class River(source: SiteId, target: SiteId)

  class Map(sites: List[Site], rivers: List[River], mines: List[SiteId]) {

  }

}
