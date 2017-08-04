package org.codingteam.icfpc2017

import java.io.EOFException

import org.codingteam.icfpc2017.Messages.{HelloRq, Punter}

/**
  * Message processing cycle.
  */
object HandlerLoop {

  def runLoop(server: StreamInterface, strategy: Strategy): Unit = {
    try {

      // TODO: greetings
//      val hello = HelloRq(Config.MyPunterName)
//      server.writeToServer(hello.toJson())
//      val helloResponse = server.readFromServer()
      //      val HelloRs(rs) = helloResponse
      // ...
      while (true) {
        // val moves = <read moves>
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
