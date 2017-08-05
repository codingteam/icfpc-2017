package org.codingteam.icfpc2017

import java.io.EOFException

import org.codingteam.icfpc2017.Messages._
import org.codingteam.icfpc2017.Common.Punter

/**
  * Message processing cycle.
  */
object HandlerLoop {

  def runLoop(server: StreamInterface, strategy: Strategy, offline: Boolean): Unit = {
    try {
      val hello = HelloRq(Config.MyPunterName)
      val helloJson = hello.toJson()
      server.writeToServer(helloJson)
      // ignoring the response - nothing interesting there
      val helloResponse = server.readFromServer()
      val setupRequest = server.readFromServer()

      val (me, map, n) = Messages.parseServerMessageJson(setupRequest) match {
        case Some(setup: SetupRq) => {
          val punter = Punter(setup.punter)
          val rs = SetupRs(punter)
          server.writeToServer(rs.toJson())
          (punter, setup.map, setup.punters)
        }
        case _ => (Punter(0), GameMap.Map.createEmpty, 0)
      }
      strategy.me = me
      strategy.map = map
      while (true) {
        val request = server.readFromServer()

        val (moves) = Messages.parseServerMessageJson(request) match {
          case Some(move: MoveRq) => {
            (move.moves)
          }
          case _ => (List[Messages.Move]())
        }
        println("Got " + moves.length + " moves to process")
        strategy.updateState(moves)
        println("State updated")
        val myMove = strategy.nextMove()
        println("Found a suitable move: " + myMove.toJson().toString)
        server.writeToServer(myMove.toJson())
      }

    } catch {
      case e: EOFException =>
        println("Exit during EOF from server")
    } finally {
      server.close()
    }
  }

}
