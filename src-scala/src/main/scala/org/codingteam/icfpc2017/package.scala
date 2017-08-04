package org.codingteam

import org.codingteam.icfpc2017.Messages.Move
import org.json4s.JsonAST.JValue

package object icfpc2017 {

  /**
    * Интерфейс стратегии для выбора хода.
    */
  trait Strategy {

    private var _map: GameMap.Map = _

    def map: GameMap.Map = _map

    def setMap(map: GameMap.Map): Unit = _map = map

    def nextMove(): Move

    def updateState(moves: Seq[Move])

    /**
      * Вероятность того, что данная стратегия сможет сделать хороший ход.
      * Эти вероятности учитываются нечётко, т.е. если стратегии имеют близкую
      * вероятность, то может быть выбрана любая.
      *
      * @return вероятность [0..1].
      */
    def goodMoveProbability(): Double

    // TODO: save/restore state methods.
  }

  /**
    * Интерфейс классов, реализующих ввод/вывод (с сервером или в pipe).
    */
  trait StreamInterface extends AutoCloseable {

    def readFromServer(): JValue

    def writeToServer(data: JValue)

    def close(): Unit
  }

  object Parsing {

    object I {
      def unapply(arg: String): Option[Int] = {
        try {
          Some(arg.toInt)
        } catch {
          case _: NumberFormatException => None
        }
      }
    }

  }

  object Config {
    val MyPunterName = "codingpunter"
  }
}
