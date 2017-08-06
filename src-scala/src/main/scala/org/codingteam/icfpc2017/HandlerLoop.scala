package org.codingteam.icfpc2017

import java.io.EOFException

import org.codingteam.icfpc2017.Common.Punter
import org.codingteam.icfpc2017.Messages._

/**
  * Message processing cycle.
  */
object HandlerLoop extends Logging {

  def runLoop(server: StreamInterface, strategy: Strategy, name: String): Unit = {
    try {
      val hello = HelloRq(name)
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

        val response = Messages.parseServerMessageJson(request) match {
          case Some(move: MoveRq) => {
            strategy.updateState(move.moves)
            strategy.nextMove()
          }
          case Some(stop: Stop) => {
            log.info("Our score: " + stop.getScore(me))
            return
          }
          case _ => Pass(me)
        }
        server.writeToServer(response.toJson())
      }

    } catch {
      case e: EOFException =>
        log.error("Exit during EOF from server", e)
    } finally {
      server.close()
    }
  }

  def runOfflineMove(server: StreamInterface, strategy: Strategy, name: String): Unit = {
    try {
      val hello = HelloRq(name)
      val helloJson = hello.toJson()
      server.writeToServer(helloJson)
      // ignoring the response - nothing interesting there
      val helloResponse = server.readFromServer()

      val request = server.readFromServer()
      Messages.parseServerMessageJson(request) match {
        case Some(setup: SetupRq) =>
          val punter = Punter(setup.punter)
          strategy.me = punter
          strategy.map = setup.map
          // TODO: save setup.punters in strategy.
          val state = strategy.state
          val rs = SetupRs(punter, state)
          server.writeToServer(rs.toJson())
        case Some(move: MoveRq) =>
          strategy.updateState(move.moves)
          val nextMove = strategy.nextMove()
          val state = strategy.state
          nextMove.state = state
          server.writeToServer(nextMove.toJson)
        case Some(stop: Stop) =>
        //          val state = stop.state
        // TODO: read punter from state and log score.
        //          val me = Punter(0)
        //          println(s"Our score: " + stop.getScore(me))
        case _ =>
      }

    } catch {
      case e: EOFException =>
      // TODO: log error.
      //        println("Exit during EOF from server")
    } finally {
      server.close()
    }
  }

}
