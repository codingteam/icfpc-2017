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
          val map = setup.map
          val n = setup.punters
          println("Our punter id is " + setup.punter + " and there's " + n + " punters in total")
          val rs = SetupRs(punter)
          server.writeToServer(rs.toJson())
          (punter, map, n)
        }
        case _ => (Punter(0), GameMap.Map.createEmpty, 0)
      }
      strategy.me = me
      strategy.map = map
      while (true) {
        val request = server.readFromServer()
        val response = Pass(Punter(0))
        server.writeToServer(response.toJson())

        // val moves = <read moves>
        // < update map >
        // strategy.updateState(moves)
        // val myMove = strategy.nextMove()
        // server.write( <myMove as json> )
      }

    } catch {
      case e: EOFException =>
        println("Exit during EOF from server")
    } finally {
      server.close()
    }
  }

}
