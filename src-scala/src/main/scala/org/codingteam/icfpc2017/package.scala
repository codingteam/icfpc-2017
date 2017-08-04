package org.codingteam

import org.json4s.jackson.Json

package object icfpc2017 {

  /**
    * Интерфейс стратегии для выбора хода.
    */
  trait Strategy {

    private var _map: GameMap.Map = _

    def map: GameMap.Map = _map

    def setMap(map: GameMap.Map): Unit = _map = map

    def nextMove(): Move

    def updateState(moves: Seq[PlayerMove])

    /**
      * Вероятность того, что данная стратегия сможет сделать хороший ход.
      * Эти вероятности учитываются нечётко, т.е. если стратегии имеют близкую
      * вероятность, то может быть выбрана любая.
      *
      * @return вероятность [0..1].
      */
    def goodMoveProbability(): Double

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
