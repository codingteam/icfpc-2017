package org.codingteam.icfpc2017

import org.codingteam.icfpc2017.Messages.{Move, Pass, Punter}

/**
  * Стратегия-заглушка. Нужна только для успешного запуска.
  * Не используйте её в реальном коде.
  */
@deprecated(message = "Do not use it!!!")
class DummyStrategy extends Strategy {
  override def nextMove(): Move = Pass(Punter.Me)

  override def updateState(moves: Seq[Move]): Unit = {}

  override def goodMoveProbability(): Double = 0.01
}
