package org.codingteam.icfpc2017

import org.codingteam.icfpc2017.GameMap._
import org.codingteam.icfpc2017.Common.Punter

import scalax.collection.mutable.Graph
import scalax.collection.mutable.EdgeOps
import scalax.collection.GraphEdge._
import scalax.collection.edge.LUnDiEdge
import scalax.collection.edge.LBase.LEdgeImplicits

case class GraphMap(var graph : Graph[Node, LUnDiEdge]) {
  def getNodes() : Iterable[Node] = {
    graph.nodes.map {
      node : Graph[Node, LUnDiEdge]#NodeT => node.value.asInstanceOf[Node]
    }
  }

  def getMines() : Iterable[Mine] = {
    getNodes().collect {case mine : Mine => mine}
  }

  def getMineNodes() : Iterable[Graph[Node, LUnDiEdge]#NodeT] = {
    graph.nodes.filter {
      node : Graph[Node, LUnDiEdge]#NodeT => node.value.isInstanceOf[Mine]
    }
  }

  def getSiteNodes() : Iterable[Graph[Node, LUnDiEdge]#NodeT] = {
    graph.nodes.filter {
      node : Graph[Node, LUnDiEdge]#NodeT => node.value.isInstanceOf[Site]
    }
  }

  def mark(source : SiteId, target : SiteId, punter : Punter) : Unit = {
    val edges = graph.edges.toList
    edges.foreach {
      edge : Graph[Node, LUnDiEdge]#EdgeT =>
        if (edge == LUnDiEdge(source,target)(None)) {
          print("hello")
        }
    }
  }
}

object GraphMap {
  def fromMap(map : Map) : GraphMap = {
    return GraphMap(map.toGraph())
  }
}
