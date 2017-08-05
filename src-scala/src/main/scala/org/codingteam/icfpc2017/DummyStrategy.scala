package org.codingteam.icfpc2017

import org.codingteam.icfpc2017.Common.Punter
import org.codingteam.icfpc2017.Messages.{Move, Pass}
import org.json4s.JsonAST.{JObject, JValue}

/**
  * Стратегия-заглушка. Нужна только для успешного запуска.
  * Не используйте её в реальном коде.
  */
@deprecated(message = "Do not use it!!!", since = "0.1-SNAPSHOT")
class DummyStrategy extends Strategy {
  override def nextMove(): Move = Pass(Punter(0))

  override def updateState(moves: Seq[Move]): Unit = {}

  override def goodMoveProbability(): Double = 0.01

  override def state: JValue = JObject()

  override def state_=(s: JValue): Unit = {}
}
