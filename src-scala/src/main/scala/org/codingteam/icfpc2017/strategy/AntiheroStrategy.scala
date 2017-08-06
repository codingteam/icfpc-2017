package org.codingteam.icfpc2017.strategy
import org.codingteam.icfpc2017.Common.Punter
import org.codingteam.icfpc2017.{Common, Logging, Messages}

import scala.util.Random

class AntiheroStrategy extends GreedyStrategy with Logging {

  override lazy val me: Common.Punter = {
    val count = commonState.punterCount
    val rnd = Random.nextInt(count - 1)
    val index = if (rnd < commonState.me.id) rnd else rnd + 1
    log.info(s"I'll be $index")
    Punter(index)
  }

  override def nextMove(): Messages.Move = {
    val move = super.nextMove()
    move match {
      case Messages.Claim(_, s, t) => Messages.Claim(commonState.me, s, t)
      case _: Messages.Pass => Messages.Pass(commonState.me)
    }
  }

  override def goodMoveProbability(): Double = super.goodMoveProbability() * 0.1

  override def updateState(moves: Seq[Messages.Move]): Unit = super.updateState(moves)
}
