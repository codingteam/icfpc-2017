package org.codingteam.icfpc2017

import java.io.{InputStream, OutputStream}

import org.codingteam.icfpc2017.Messages.Move

import scala.util.Random

/**
  * Основная стратегия, делегирующая выбор хода стратегии из списка.
  * Выбирается та стратегия, для которой текущая ситуация наиболее удобна.
  */
class DelegatingStrategy(val strategies: Seq[Strategy]) extends Strategy {
  require(strategies.nonEmpty)

  private val rnd = new Random()

  override def commonState_=(s: CommonState): Unit = {
    super.commonState_=(s)
    strategies foreach (_.commonState = s)
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

  override def read(is: InputStream): Unit = {
    strategies foreach (_.read(is))
  }

  override def write(os: OutputStream): Unit = {
    strategies foreach (_.write(os))
  }
}
