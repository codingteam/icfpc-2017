package org.codingteam.icfpc2017

import java.io.EOFException

import org.codingteam.icfpc2017.Messages.{HelloRq, Punter, SetupRq}

/**
  * Message processing cycle.
  */
object HandlerLoop {

  def runLoop(server: StreamInterface, strategy: Strategy): Unit = {
    try {
      val hello = HelloRq(Config.MyPunterName)
      val helloJson = hello.toJson()
      server.writeToServer(helloJson)
      // ignoring the response - nothing interesting there
      val helloResponse = server.readFromServer()
      val setupRequest = server.readFromServer()
      var punter: Punter = new Punter(0)
      var map: GameMap.Map =
        new GameMap.Map(List[GameMap.Site](),
          List[GameMap.River](),
          List[GameMap.SiteId]())
      var n: BigInt = 0
      Messages.parseServerMessageJson(setupRequest) match {
        case Some(setup: SetupRq) => {
          punter = new Punter(setup.punter)
          map = setup.map
          n = setup.punters
          println("Our punter id is " + setup.punter + " and there's " + n + " punters in total")
        }
        case default => ()
      }
      while (true) {
        val request = server.readFromServer()

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
