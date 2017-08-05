package org.codingteam.icfpc2017

import org.codingteam.icfpc2017.GameMap.Map
import org.codingteam.icfpc2017.Messages.Move
import org.codingteam.icfpc2017.Common.Punter
import org.json4s.JsonAST.{JObject, JValue}

import scala.util.Random

/**
  * Основная стратегия, делегирующая выбор хода стратегии из списка.
  * Выбирается та стратегия, для которой текущая ситуация наиболее удобна.
  */
class DelegatingStrategy(val strategies: Seq[Strategy]) extends Strategy {
  require(strategies.nonEmpty)

  private val rnd = new Random()

  override def map_=(map: Map): Unit = {
    super.map_=(map)
    strategies foreach (_.map_=(map))
  }

  override def me_=(punter: Punter): Unit = {
    super.me_=(punter)
    strategies foreach (_.me = punter)
  }

  override def nextMove(): Move = {
    val ps = strategies.map(s => (s, s.goodMoveProbability())) sortBy (v => -v._2)
    // случайно выбираем из наилучших стратегий, если они имеют близкие вероятности.
    val best = ps.head
    val Accuracy = 0.05
    val good = ps.takeWhile(_._2 >= best._2 - Accuracy)
    assert(good.nonEmpty)
    good(rnd nextInt good.size)._1.nextMove()
  }

  override def updateState(moves: Seq[Move]): Unit = {
    strategies foreach (_.updateState(moves))
  }

  override def goodMoveProbability(): Double = 0.95 // no ideas about actual value in this case.

  override def state: JValue = {
    // TODO: implement this.
    JObject()
  }

  override def state_=(s: JValue): Unit = {
    // TODO: implement this.
  }
}
