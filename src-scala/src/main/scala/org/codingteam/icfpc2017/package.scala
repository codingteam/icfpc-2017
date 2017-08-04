package org.codingteam

import org.json4s.jackson.Json

package object icfpc2017 {

  /**
    * Интерфейс стратегии для выбора хода.
    */
  trait Strategy {

    def nextMove(): Move

    def updateState(moves: Seq[PlayerMove])

  }

  case class PlayerMove()

  case class Move()

  /**
    * Интерфейс классов, реализующих ввод/вывод (с сервером или в pipe).
    */
  trait StreamInterface {

    def readFromServer(): Json

    def writeToServer(data: Json)

  }

}
