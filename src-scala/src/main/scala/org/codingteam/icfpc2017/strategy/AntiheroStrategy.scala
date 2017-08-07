package org.codingteam.icfpc2017.strategy

import org.codingteam.icfpc2017.Common.Punter
import org.codingteam.icfpc2017.{Canceller, Common, Logging, Messages}

import scala.util.Random

class AntiheroStrategy extends GreedyStrategy with Logging {

  override lazy val me: Common.Punter = {
    val count = commonState.punterCount
    val rnd = Random.nextInt(count - 1)
    val index = if (rnd < commonState.me.id) rnd else rnd + 1
    log.info(s"I'll be $index")
    Punter(index)
  }

  override def nextMove(deadLineMs: Long, cancel: Canceller): Messages.Move = {
    val move = super.nextMove(deadLineMs, cancel)
    move match {
      case m: Messages.Claim => m.copy(punter = commonState.me)
      case m: Messages.Pass => m.copy(punter = commonState.me)
      case m: Messages.Splurge => m.copy(punter = commonState.me)
      case m: Messages.AnOption => m.copy(punter = commonState.me)
    }
  }

  override def goodMoveProbability(): Double = super.goodMoveProbability() * 0.5

  override def updateState(moves: Seq[Messages.Move]): Unit = super.updateState(moves)
}
