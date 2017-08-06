package org.codingteam.icfpc2017

import org.codingteam.icfpc2017.GameMap.Node
import org.codingteam.icfpc2017.Messages.{Move, Pass}

import scala.collection.mutable.{Map => MMap, Set => MSet}
import scala.util.Random

/**
  * Created by portnov on 8/5/17.
  */
class ComponentConnectorStrategy extends Strategy {

  private var rng = Random

  def getComponents() : Seq[Iterable[Node]] = {
      val g = graph.graph
      val subgraph = g filter g.having(edge = {
        edge: g.EdgeT => (edge.label != None) && (edge.label == me)
      })
      val components =
        (for (c <- subgraph.componentTraverser())
          yield c.nodes.map(_.value)
        ).toSeq
      //println(s"Components: $components")
      components
  }

  override def nextMove(): Move = {
    val g = graph.graph
    val freeSubgraph = g filter g.having(edge = {
      edge: g.EdgeT => edge.label == None
    })
    //println(s"Free subgraph: $freeSubgraph")

    var candidates: List[freeSubgraph.EdgeT] = List()
    val components = getComponents()
    val componentsNumber = components.size
    /*println(s"Found components: ${components.size}")
    for (c <- components) {
      println(c)
    }*/
    if (componentsNumber > 1) {
      val component1Idx = rng.nextInt(componentsNumber)
      val component2Idx = (component1Idx + 1) % componentsNumber
      val component1 = components(component1Idx)
      val component2 = components(component2Idx)

      assert(! component1.isEmpty)
      assert(! component1.isEmpty)

      var selectedPair : Option[(Node,Node)] = None
      var bestPath : Option[freeSubgraph.Path] = None
      var bestRho = 1000500

      component1.foreach({
        node1: Node => component2.foreach({
          node2: Node => {
            val n1Opt = freeSubgraph find node1
            val n2Opt = freeSubgraph find node2
            (n1Opt, n2Opt) match {
              case (Some(n1), Some(n2)) =>
                (n1 shortestPathTo n2) match {
                  case None => // println(s"No way: $n1 - $n2")
                  case Some(path) =>
                    if (path.length < bestRho) {
                      bestPath = Some(path)
                      bestRho = path.length
                      selectedPair = Some (node1, node2)
                      //println(s"Found better pair: $node1 - $node2 with distance $bestRho")
                    }
                }
              case _ => // println(s"Nodes not in free subgraph: $node1 - $node2")
            }
          }
        })
      })

      selectedPair match {
        case None => println("Can not select a pair of nodes that could be connected.")
        case Some(bestNodes) =>
          val node1Opt = freeSubgraph find bestNodes._1
          val node2Opt = freeSubgraph find bestNodes._2
          (node1Opt, node2Opt) match {
            case (Some(node1), Some(node2)) =>
              bestPath match {
                case None => println(s"Both selected nodes $node1, $node2 belong to free subgraph, but there is no free way between them.")
                case Some(path) => {
                  println(s"Found path: $node1 - $node2 :: ${path.length}")
                  if (path.length == 1) {
                    println("Will connect two components.")
                  }
                  //val edge1 = (g find LUnDiEdge(node1.value, path.nodes.head.value)(me)).get
                  //val edge2 = (g find LUnDiEdge(path.nodes.last.value, node2.value)(me)).get
                  candidates = List(path.edges.head, path.edges.last)
                  //candidates = List(edge1, edge2)
                }
              }
            case _ => println("No ways to connect components found, this is odd.")
          }
      }
    }

    if (candidates.isEmpty) {
      println("Component connector can not find good move")
      Pass(me)
    } else {
      val index = rng.nextInt(candidates.size)
      val edge = candidates.toIndexedSeq(index)

      val from = edge._1.value match {
        case x@GameMap.Site(id) => x
        case GameMap.Mine(id) => GameMap.Site(id)
      }
      val to = edge._2.value match {
        case x@GameMap.Site(id) => x
        case GameMap.Mine(id) => GameMap.Site(id)
      }

      val sourceNode = map.siteToNode(from)
      val targetNode = map.siteToNode(to)
      graph.mark(sourceNode, targetNode, me)
      val score = graph.score(me)
      val our = graph.getPunterEdges(me).size
      val total = graph.graph.edges.size
      println(s"Our expected score: $score, our edges: $our, total edges: $total")
      Messages.Claim(me, from, to)
    }
  }

  override def goodMoveProbability(): Double = {
    val g = graph.graph
    val subgraph = g filter g.having(edge = {
      edge: g.EdgeT => (edge.label != None) && (edge.label == me)
    })
    val componentsNumber = getComponents().size
    println(s"Found components: ${componentsNumber}")
    if (componentsNumber > 1) {
      /*for (c <- components) {
        println(c)
      }*/
      componentsNumber
    } else {
      0.0
    }
  }

  override def updateState(moves: Seq[Move]): Unit = {}
}
