package org.codingteam

import org.codingteam.icfpc2017.Messages.Move
import org.codingteam.icfpc2017.Common.Punter
import org.json4s.JsonAST.JValue

package object icfpc2017 {

  /**
    * Интерфейс стратегии для выбора хода.
    */
  trait Strategy {

    private var _map: GameMap.Map = GameMap.Map.createEmpty

    def map: GameMap.Map = _map

    def map_=(map: GameMap.Map): Unit = _map = map

    private var _me: Punter = Punter(0)

    def me = _me

    def me_=(punter: Punter) = _me = punter

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

    /**
      * Serialize state to JValue.
      * NOTE: you should remember that json is slow.
      *
      * @return state.
      */
    def state: JValue

    /**
      * Import state from JValue.
      * NOTE: you should remember that json is slow.
      *
      * @param s state.
      */
    def state_=(s: JValue)
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

  implicit class BooleanExt(val b: Boolean) extends AnyVal {
    def toOption[T](v: => T): Option[T] = if (b) Some(v) else None
  }

}
